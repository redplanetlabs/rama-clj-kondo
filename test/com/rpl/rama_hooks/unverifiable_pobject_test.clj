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

(deftest depot-registry-sources-hook-test
  ;; Tests that *-prefixed depot vars registered via declare-depot or
  ;; declare-tick-depot emit info diagnostics when used in standalone <<sources.
  ;; Unregistered *-prefixed vars should not produce info diagnostics.
  (testing "sources-hook with known-depot-names registry"
           (with-testing-context
            "emits info diagnostic for depot var registered via declare-depot"
             (reset! @#'rama/known-depot-names #{})
             (rama/declare-depot-hook
              (sexpr->node '(declare-depot setup *transaction-depot (hash-by :x))))
             (rama/sources-hook
              (sexpr->node
               '(<<sources s
                           (source> *transaction-depot :> *data)
                           (println *data))))
             (is (= 1 (count (get-info-findings)))
                 "should emit exactly one info finding for the known depot var")
             (is (= [:rama-unverifiable-pobject] (get-info-types))
                 "finding type should be :rama-unverifiable-pobject")
             (is (= ["Cannot verify that *transaction-depot is declared in this module's scope"]
                    (get-info-findings))))

           (with-testing-context
            "emits info diagnostic for depot var registered via declare-tick-depot"
             (reset! @#'rama/known-depot-names #{})
             (rama/declare-tick-depot-hook
              (sexpr->node '(declare-tick-depot setup *cleanup-tick 60000)))
             (rama/sources-hook
              (sexpr->node
               '(<<sources s
                           (source> *cleanup-tick :> *data)
                           (println *data))))
             (is (= 1 (count (get-info-findings)))
                 "should emit exactly one info finding for the known depot var")
             (is (= [:rama-unverifiable-pobject] (get-info-types))
                 "finding type should be :rama-unverifiable-pobject")
             (is (= ["Cannot verify that *cleanup-tick is declared in this module's scope"]
                    (get-info-findings))))

           (with-testing-context
            "does not emit info diagnostic for *var not in depot registry"
             (reset! @#'rama/known-depot-names #{})
             (rama/sources-hook
              (sexpr->node
               '(<<sources s
                           (source> *depot :> *data)
                           (identity *unknown-var :> *result))))
             (is (= [] (get-info-findings))
                 "should emit no info findings for unregistered *-prefixed vars"))))
