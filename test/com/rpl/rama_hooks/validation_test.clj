(ns com.rpl.rama-hooks.validation-test
    (:require
     [clj-kondo.impl.utils :as utils]
     [clojure.test :refer [deftest is]]
     [com.rpl.errors :as err]
     [com.rpl.rama-hooks :as rama]
     [com.rpl.test-helpers :refer [body->sexpr
                                   node->sexpr
                                   sexpr->node
                                   transform-sexprs
                                   with-testing-context]]))

(deftest keywords-as-fns-are-illegal-in-dataflow-test
  (with-testing-context
   "Can't have fn appear in foreign-select contexts"

    (binding [rama/*context* :dataflow]

             (body->sexpr
              (transform-sexprs
               '(identity {:a 1 :b 2} :> *map)
               '(:a *map :> *val)))

             (is (= err/syntax-error-keyword-fn-in-dataflow
                    (-> utils/*ctx*
                        :findings
                        deref
                        first
                        :message))))))

(deftest lambda-are-illegal-in-multiple-contexts-test
  (with-testing-context
   "Can't have fn appear in foreign-select contexts"
    (rama/foreign-select-hook
     (sexpr->node '(foreign-select [:x (view (fn [x] (inc x)))]
                                   $$pstate)))
    (is (= err/syntax-error-lambda-fn-in-foreign-select
           (-> utils/*ctx*
               :findings
               deref
               first
               :message))))

  (with-testing-context
   "Can't use fn in dataflow code contexts"
    (binding [rama/*context* :dataflow]
             (body->sexpr
              (transform-sexprs
               '(foreign-select [:x (view (fn [x] (inc x)))]
                                $$pstate)))
             (is (= (err/syntax-error-illegal-special-form 'fn)
                    (-> utils/*ctx*
                        :findings
                        deref
                        first
                        :message))))))

(deftest special-forms-are-illegal-in-dataflow-test
  (binding [rama/*context* :dataflow]
           (doseq [form rama/illegal-forms]
                  (with-testing-context
                   (str "Can't use " form " in dataflow code.")

                    (body->sexpr
                     (transform-sexprs
                      '(when true? (prn "true"))))

                    (is (= (err/syntax-error-illegal-special-form 'when)
                           (-> utils/*ctx*
                               :findings
                               deref
                               first
                               :message)))))))

(deftest java-method-calls-and-constructors-are-illegal-in-dataflow-test
  (with-testing-context
   "Java constructors and method calls are illegal in dataflow code"

    (binding [rama/*context* :dataflow]

             (body->sexpr
              (transform-sexprs
               '(HashMap. :> *map)
               '(.set *map :a 1)))

             (is (= [err/syntax-error-java-interop-in-dataflow
                     err/syntax-error-java-interop-in-dataflow]
                    (->> utils/*ctx*
                         :findings
                         deref
                         (mapv :message)))))))

(def bank-transfer-module-with-error-code
     '(defmodule BankTransferModule
                 [setup topologies]
                 (declare-depot setup *transfer-depot (hash-by :from-user-id))
                 (let [mb (microbatch-topology topologies "banking")]
                      (declare-pstate mb $$funds {Long Long})
                      (<<sources
                       mb
                       (source> *transfer-depot :> %microbatch)
                       (%microbatch :> {:keys [*success? *from-user-id *amt]})
                       (identity {:a 1 :b 2} :> *map)
                       (:a *map :> *a)
                       (<<if
                        *success?
                        (<<ramafn
                         %deduct [*curr]
                         (:> (- *curr *amt)))
                        (local-transform>
                         [(keypath *from-user-id) (term %deduct)] $$funds))))))

(def bank-transfer-module-with-error-code-transformed
     '(defn BankTransferModule
            [setup topologies]
            (let [*transfer-depot (declare-depot setup (hash-by :from-user-id))]
                 (pr *transfer-depot)
                 (let [mb (microbatch-topology topologies "banking")]
                      (let [$$funds (declare-pstate mb {Long Long})]
                           (pr $$funds)
                           (fn
                            []
                            (let [%microbatch (source> *transfer-depot)]
                                 (let [{:keys [*success? *from-user-id *amt]} (%microbatch)]
                                      (let [*map (identity {:a 1 :b 2})]
                                           (let [*a (:a *map)]
                                                (when *success?
                                                      (letfn
                                                       [(%deduct [*curr] (:> (- *curr *amt)))]
                                                       (local-transform>
                                                        [(keypath *from-user-id)
                                                         (term %deduct)]
                                                        $$funds)))))))))))))

(deftest bank-transfer-module-with-errors-test
  (with-testing-context
   "Should error in multiple places"
    (is (= bank-transfer-module-with-error-code-transformed
           (node->sexpr
            (rama/defmodule-hook
             (sexpr->node bank-transfer-module-with-error-code)))))))
