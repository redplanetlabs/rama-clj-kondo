(ns com.rpl.rama-hooks.module-code-test
  (:require
   [clj-kondo.hooks-api :as api]
   [clojure.test :refer [deftest is testing]]
   [com.rpl.errors :as err]
   [com.rpl.rama-hooks :as rama]
   [com.rpl.test-helpers :refer [get-error-messages
                                  sexpr->node
                                  node->sexpr
                                  transform-module-sexprs
                                  ->sexpr
                                  with-testing-context]]))

(deftest module-hook-test
  ;; Test module-hook transformation of module code (declare-depot, declare-pstate)
  ;; Contracts: transforms linear module forms into nested let expressions
  (testing "module-hook"
    (testing "Basic expressions within a module"
      (is (= '(let
               [*depot (declare-depot setup (hash-by :x))]
               (pr *depot))
             (api/sexpr
              (first
               (rama/transform-module-form
                (->sexpr '(declare-depot setup *depot (hash-by :x)))
                nil
                #{})))))
      (is
       (= '(let
            [*depot (declare-depot setup (hash-by :x))]
            (pr *depot)
            (let
             [*depot-2 (declare-depot setup (hash-by :y))]
             (pr *depot-2)
             (pr *depot-2)))
          (api/sexpr (first
                      (transform-module-sexprs
                       '(declare-depot setup *depot (hash-by :x))
                       '(declare-depot setup *depot-2 (hash-by :y))
                       '(pr *depot-2)))))))

    (testing "Basic anonymous module definitions"
      (let [module-def
            '(module
              [setup topologies]
              (declare-depot setup *transfer-depot (hash-by :x))
              (let
               [mb (microbatch-topology topologies "banking")]
               (declare-pstate mb $$funds {Long Long})))]
        (is
         (=
          '(fn
            [setup topologies]
            (let
             [*transfer-depot (declare-depot setup (hash-by :x))]
             (pr *transfer-depot)
             (let
              [mb (microbatch-topology topologies "banking")]
              (let
               [$$funds (declare-pstate mb {Long Long})]
               (pr $$funds)))))
          (node->sexpr
           (rama/module-hook
            (sexpr->node module-def)))))))

    (with-testing-context
     "Errors if the module input vectors is missing, or isn't a vector"
     (rama/module-hook
      (sexpr->node '(module)))

     (rama/module-hook
      (sexpr->node '(module ())))

     (rama/module-hook
      (sexpr->node '(module {:module-name "xyz"} ())))

     (rama/module-hook
      (sexpr->node '(module {:module-name "xyz"})))

     (is (= [err/syntax-error-missing-input-vector
             err/syntax-error-missing-input-vector
             err/syntax-error-missing-input-vector
             err/syntax-error-missing-input-vector]
            (get-error-messages))))))

(deftest defmodule-hook-test
  ;; Test defmodule-hook transformation of module definitions
  ;; Contracts: transforms defmodule into defn with nested let expressions
  (testing "defmodule-hook"
    (testing "Basic defmodule definitions"
      (let [module-def
            '(defmodule
              MyModule
              [setup topologies]
              (declare-depot setup *transfer-depot (hash-by :x))
              (let
               [mb (microbatch-topology topologies "banking")]
               (declare-pstate mb $$funds {Long Long})))]
        (is
         (=
          '(defn
            MyModule
            [setup topologies]
            (let
             [*transfer-depot (declare-depot setup (hash-by :x))]
             (pr *transfer-depot)
             (let
              [mb (microbatch-topology topologies "banking")]
              (let
               [$$funds (declare-pstate mb {Long Long})]
               (pr $$funds)))))
          (node->sexpr
           (rama/defmodule-hook
            (sexpr->node module-def)))))))

    (testing "Full defmodule definitions"
      (is
       (=
        '(defn
          RestAPIIntegrationModule
          [setup topologies]
          (let
           [*depot (declare-depot setup (hash-by identity))]
           (pr *depot)
           (let
            [*depot-2 (declare-depot setup (hash-by identity))]
            (pr *depot-2)
            (let
             [s (stream-topology topologies "s")]
             (let
              [$$p1 (declare-pstate s {Object Object})]
              (pr $$p1)
              (let
               [$$p2 (declare-pstate s {Object Long})]
               (pr $$p2)
               (fn
                []
                (let
                 [[*k *v] (source> *depot)]
                 (+compound $$p {*k (+vec-agg *v)}))
                (let
                 [*key (source> *depot-2)]
                 (+compound $$p {*key (+count)})))))))))
        (node->sexpr
         (rama/defmodule-hook
          (sexpr->node '(defmodule
                         RestAPIIntegrationModule
                         [setup topologies]
                         (declare-depot setup *depot (hash-by identity))
                         (declare-depot setup *depot-2 (hash-by identity))
                         (let
                          [s (stream-topology topologies "s")]
                          (declare-pstate s $$p1 {Object Object})
                          (declare-pstate s $$p2 {Object Long})
                          (<<sources
                           s
                          (source> *depot :> [*k *v])
                           (+compound $$p {*k (+vec-agg *v)})
                          (source> *depot-2 :> *key)
                           (+compound $$p {*key (+count)}))))))))))

    (with-testing-context
     "Errors if the module name or input vectors are missing, or don't have the correct type"
     (rama/defmodule-hook
      (sexpr->node '(defmodule)))

     (rama/defmodule-hook
      (sexpr->node '(defmodule 123)))

     (rama/defmodule-hook
      (sexpr->node '(defmodule 123 [])))

     (rama/defmodule-hook
      (sexpr->node '(defmodule symbol (xyz))))

     (is (= [err/syntax-error-missing-def-name
             err/syntax-error-missing-input-vector
             err/syntax-error-missing-def-name
             err/syntax-error-missing-input-vector
             err/syntax-error-missing-def-name
             err/syntax-error-missing-input-vector]
            (get-error-messages))))))
