(ns com.rpl.rama-hooks.unverifiable-pobject-test
    "Tests for info-level diagnostics on unverifiable $$-prefixed pstate references."
    (:require
     [clj-kondo.impl.utils :as utils]
     [clojure.test :refer [deftest is testing]]
     [com.rpl.rama-hooks :as rama]
     [com.rpl.test-helpers :refer [sexpr->node
                                   with-testing-context]]))

(defn- get-info-findings
       []
       (->> utils/*ctx*
            :findings
            deref
            (filter #(= :info (:level %)))
            (mapv :message)))

(defn- get-info-types
       []
       (->> utils/*ctx*
            :findings
            deref
            (filter #(= :info (:level %)))
            (mapv :type)))

(deftest query-topology-hook-unverifiable-pstate-test
  ;; Tests that standalone <<query-topology emits info diagnostics for $$-prefixed
  ;; symbols that are not locally bound within the topology body.
  (testing "query-topology-hook"
           (with-testing-context
            "emits info diagnostic for $$-prefixed symbol not locally bound"
             (rama/query-topology-hook
              (sexpr->node
               '(<<query-topology topologies "order-status" [*id :> *result]
                                  (local-select> (keypath *id) $$customer :> *result))))
             (is (= 1 (count (get-info-findings)))
                 "should emit exactly one info finding")
             (is (= [:rama-unverifiable-pobject] (get-info-types))
                 "finding type should be :rama-unverifiable-pobject")
             (is (= ["Cannot verify that $$customer is declared in this module's scope"]
                    (get-info-findings))))

           (with-testing-context
            "does not emit info diagnostic for $$-prefixed symbol locally bound via materialize>"
             (rama/query-topology-hook
              (sexpr->node
               '(<<query-topology topologies "q" [*id :> *result]
                                  (<<batch
                                   (materialize> *k *v :> $$tmp)
                                   ($$tmp :> *k2 *v2)))))
             (is (= [] (get-info-findings))
                 "should emit no info findings when pstate is locally bound"))

           (with-testing-context
            "does not emit info diagnostic for *-prefixed unresolved symbols"
             (rama/query-topology-hook
              (sexpr->node
               '(<<query-topology topologies "q" [*id :> *result]
                                  (local-select> (keypath *id) $$customer :> *v)
                                  (identity *undefined-var :> *result))))
             (is (= 1 (count (get-info-findings)))
                 "should emit info finding only for $$customer, not for *undefined-var")
             (is (= ["Cannot verify that $$customer is declared in this module's scope"]
                    (get-info-findings))
                 "info finding should be for $$customer only"))))

(deftest sources-hook-unverifiable-pstate-test
  ;; Tests that standalone <<sources emits info diagnostics for $$-prefixed
  ;; symbols that are not locally bound within the sources body.
  (testing "sources-hook"
           (with-testing-context
            "emits info diagnostic for $$-prefixed symbol not locally bound"
             (rama/sources-hook
              (sexpr->node
               '(<<sources s
                           (source> *depot :> *data)
                           (+compound $$p {*data (+count)}))))
             (is (= 1 (count (get-info-findings)))
                 "should emit exactly one info finding")
             (is (= [:rama-unverifiable-pobject] (get-info-types))
                 "finding type should be :rama-unverifiable-pobject")
             (is (= ["Cannot verify that $$p is declared in this module's scope"]
                    (get-info-findings))))))
