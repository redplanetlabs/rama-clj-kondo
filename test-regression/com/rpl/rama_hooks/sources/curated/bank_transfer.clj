(ns com.rpl.rama-hooks.sources.curated.bank-transfer
    (:require
     [com.rpl.rama :refer :all]
     [com.rpl.rama.path :refer :all]))

(defmodule BankTransferModule [setup topologies]
           (declare-depot setup *depot :random)

           (let [mb (microbatch-topology topologies "banking")]
                (declare-pstate mb $$balances {Long Long})
                (declare-pstate mb $$outgoing {Long {String Object}})
                (declare-pstate mb $$incoming {Long {String Object}})

                (<<sources mb
                           (source> *depot :> %mb)
                           (%mb :> *record)
                           (first *record :> *type)
                           (<<cond
                            (case> (= *type :deposit))
                            (nth *record 1 :> *user-id)
                            (nth *record 2 :> *amt)
                            (|hash *user-id)
                            (local-transform> [(keypath *user-id) (nil->val 0) (term (partial + *amt))] $$balances)

                            (case> (= *type :transfer))
                            (nth *record 1 :> *transfer-id)
                            (nth *record 2 :> *from)
                            (nth *record 3 :> *to)
                            (nth *record 4 :> *amt)
                            (|hash *from)
                            (local-select> [(keypath *from) (nil->val 0)] $$balances :> *balance)
                            (>= *balance *amt :> *success)
                            (<<if *success
                                  (- *balance *amt :> *new-balance)
                                  (local-transform> [(keypath *from) (termval *new-balance)] $$balances)
                                  (else>))
                            (local-transform> [(keypath *from) (nil->val {}) (keypath *transfer-id)
                                               (termval {:to-user-id *to :amt *amt :success? *success})] $$outgoing)
                            (|hash *to)
                            (<<if *success
                                  (local-select> [(keypath *to) (nil->val 0)] $$balances :> *to-balance)
                                  (+ *to-balance *amt :> *new-to-balance)
                                  (local-transform> [(keypath *to) (termval *new-to-balance)] $$balances)
                                  (else>))
                            (local-transform> [(keypath *to) (nil->val {}) (keypath *transfer-id)
                                               (termval {:from-user-id *from :amt *amt :success? *success})] $$incoming))))

           (<<query-topology topologies "get-balance" [*user-id :> *balance]
                             (|hash *user-id)
                             (local-select> [(keypath *user-id) (nil->val 0)] $$balances :> *balance)
                             (|origin))

           (<<query-topology topologies "get-outgoing" [*user-id :> *result]
                             (|hash *user-id)
                             (local-select> [(keypath *user-id) (nil->val {})] $$outgoing :> *transfers)
                             (vec *transfers :> *result)
                             (|origin))

           (<<query-topology topologies "get-incoming" [*user-id :> *result]
                             (|hash *user-id)
                             (local-select> [(keypath *user-id) (nil->val {})] $$incoming :> *transfers)
                             (vec *transfers :> *result)
                             (|origin)))
