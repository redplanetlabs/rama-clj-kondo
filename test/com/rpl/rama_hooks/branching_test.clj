(ns com.rpl.rama-hooks.branching-test
  (:require
   [clj-kondo.hooks-api :as api]
   [clj-kondo.impl.utils :as utils]
   [clojure.test :refer [deftest is testing]]
   [com.rpl.errors :as err]
   [com.rpl.test-helpers :refer [body->sexpr get-error-messages
                                  get-first-error-message sexpr->node
                                  transform-sexprs with-testing-context]]))

;;; Tests for Rama branching constructs: <<if, <<switch, <<cond, <<sources, <<subsource
;;; These forms split the dataflow into distinct code paths and support variable unification

(deftest <<if-test
  ;; Test <<if construct transformation and unification
  ;; Contract: transforms <<if to if/when with proper branch handling and variable unification

  (testing "<<if"
    (testing "Happy path"
      (is (= '(when true (one) (two) (three))
             (body->sexpr
              (transform-sexprs
               '(<<if true (one) (two) (three))))))
      (is (= '(if true (do (one) (two)) (three))
             (body->sexpr
              (transform-sexprs
               '(<<if true (one) (two) (else>) (three)))))))

    (testing "Unification"
      (is
       (= '(let
            [*x nil
             _
             (if
              true
              (let [*x (identity 1)] {*x *x})
              (let [*x (identity 2)] {*x *x}))]
            (let [*y (+ *x 1)]))
          (body->sexpr
           (transform-sexprs
            '(<<if true (identity 1 :> *x) (else>) (identity 2 :> *x))
            '(+ *x 1 :> *y)))))

      (is
       (= '(let
            [*b nil
             _
             (if
              true
              (do
                (let [*a (identity 1)]
                  (let [*b (identity 2)]
                    {*b *b})))
              (do
                (let [*b (identity 3)]
                  (let [*c (identity 4)]
                    {*b *b}))))]
            (let [*d (+ *b 1)]
              (let [*e (+ *d 1)]
                (pr *e))))
          (body->sexpr
           (transform-sexprs
            '(<<if
              true
              (identity 1 :> *a)
              (identity 2 :> *b)
              (else>)
              (identity 3 :> *b)
              (identity 4 :> *c))
            '(+ *b 1 :> *d)
            '(+ *d 1 :> *e)
            '(pr *e)))))

      (is
       (= '(let
            [*x nil
             _
             (if
              true
              (do (prn "if") (let [*x (identity 1)] {*x *x}))
              (do (prn "else") (let [*x (identity 2)] {*x *x})))]
            (let [*y (+ *x 1)]))
          (body->sexpr
           (transform-sexprs
            '(<<if
              true
              (prn "if")
              (identity 1 :> *x)
              (else>)
              (prn "else")
              (identity 2 :> *x))
            '(+ *x 1 :> *y))))))

    (testing "Nested conditional unification"
      (is
       (=
        '(let
          [*b nil
           _ (if true
               (do
                 (prn "if")
                 (let
                  [*a (identity 1)]
                  (prn *a)
                  (let
                   [*b (identity 2)]
                   {*b *b})))
               (do
                 (prn "else")
                 (let
                  [*b nil
                   _ (cond
                       (case> *x)
                       (let [*b (identity 3)] {*b *b})
                       (case> *y)
                       (let [*b (identity 4)] {*b *b})
                       (default>)
                       (let [*b (identity 5)] {*b *b}))]
                  (prn *b)
                  (let
                   [*c (identity 6)]
                   {*b *b}))))]
          (prn *b))
        (body->sexpr
         (transform-sexprs
          '(<<if true
             (prn "if")
             (identity 1 :> *a)
             (prn *a)
             (identity 2 :> *b)
             (else>)
             (prn "else")
             (<<cond
              (case> *x)
              (identity 3 :> *b)
              (case> *y)
              (identity 4 :> *b)
              (default>)
              (identity 5 :> *b))
             (prn *b)
             (identity 6 :> *c))
          '(prn *b))))))

    (with-testing-context
     "Can't have multiple else>s"
     (is (= '(if true (one) (two))
            (-> '(<<if true (one) (else>) (two) (else>) (three))
                (transform-sexprs)
                body->sexpr)))
     (is (= err/syntax-error-multiple-elses
            (-> utils/*ctx*
                :findings
                deref
                first
                :message))))

    (with-testing-context
     "(else>) should have no arguments"
     (= '(if true (one) (three))
        (-> (transform-sexprs '(<<if true (one) (else> (two)) (three)))
            (first)
            (api/sexpr)))
     (is (= err/syntax-error-else>-arity
            (-> utils/*ctx*
                :findings
                deref
                first
                :message))))))

(deftest <<switch-test
  ;; Test <<switch construct transformation and unification
  ;; Contract: transforms <<switch to cond with case> clauses and variable unification

  (testing "<<switch"
    (testing "Happy path"
      (is (= '(cond (case> (= :x :y)) (do (one) (two)) (case> (= :x :x)) (three))
             (body->sexpr
              (transform-sexprs
               '(<<switch :x (case> :y) (one) (two) (case> :x) (three)))))))

    (testing "Unification"
      (is
       (=
        '(let
          [*x nil
           _
           (cond
            (case> (= :x :y))
            (let [*x (identity 1)] {*x *x})
            (case> (= :x :x))
            (let [*x (identity 2)] {*x *x}))])
        (body->sexpr
         (transform-sexprs
          '(<<switch
            :x
            (case> :y)
            (identity 1 :> *x)
            (case> :x)
            (identity 2 :> *x)))))))

    (with-testing-context
     "Arity check on (case> ...)"
     (body->sexpr
      (transform-sexprs
       '(<<switch :x (case> :x :y) (one) (two) (case> true) (three))))
     (is (= err/syntax-error-case>-arity
            (-> utils/*ctx*
                :findings
                deref
                first
                :message))))))

(deftest <<cond-test
  ;; Test <<cond construct transformation with default> and unification
  ;; Contract: transforms <<cond to cond with case> and default> clauses

  (testing "<<cond"
    (testing "Happy path"
      (is
       (=
        '(cond
          (case> false)
          (do
            (pr 1)
            (pr (two)))
          (case> true)
          (three))
        (body->sexpr
         (transform-sexprs
          '(<<cond
            (case> false)
            (pr 1)
            (pr (two))
            (case> true)
            (three)))))))

    (testing "default>"
      (is
       (= '(cond
            (case> false)
            (do
              (pr 1)
              (pr (two)))
            (default>)
            (three))
          (body->sexpr
           (transform-sexprs
            '(<<cond
              (case> false)
              (pr 1)
              (pr (two))
              (default>)
              (three)))))))


    (testing "Unification"
      (is
       (=
        '(let
          [*x nil
           _
           (cond
            (case> false)
            (do
              (pr 1)
              (let [*x (identity 1)] {*x *x}))
            (case> true)
            (do
              (pr 2)
              (let [*x (identity 2)] {*x *x})))])
        (body->sexpr
         (transform-sexprs
          '(<<cond
            (case> false)
            (pr 1)
            (identity 1 :> *x)
            (case> true)
            (pr 2)
            (identity 2 :> *x)))))))

    (testing "default> unification"
      (is
       (= '(let
            [*x nil
             _
             (cond
              (case> false)
              (let [*x (identity 1)] {*x *x})
              (default> :unify false)
              (pr {*x *x}))]
            (pr *x))
          (body->sexpr
           (transform-sexprs
            '(<<cond
              (case> false)
              (identity 1 :> *x)
              (default> :unify false)
              (pr))
            '(pr *x))))))

    (with-testing-context
     "Arity check on (case> ...)"
     (body->sexpr
      (transform-sexprs
       '(<<cond (case> false true) (one) (two) (case> true) (three))))
     (is (= err/syntax-error-case>-arity
            (-> utils/*ctx*
                :findings
                deref
                first
                :message))))

    (testing "Make sure the `do` doesn't get trampolined"
      (is
       (=
        '(cond
          (trampoline case> false)
          (do
            (trampoline pr 1)
            (trampoline pr (trampoline two)))
          (trampoline case> true)
          (trampoline three))
        (api/sexpr
         (first
          (transform-sexprs
           '(<<cond
             (case> false)
             (pr 1)
             (pr (two))
             (case> true)
             (three))))))))))

(deftest <<sources-test
  ;; Test <<sources construct for both streaming and microbatch patterns
  ;; Contract: transforms <<sources into fn with source> bindings

  (testing "<<sources"
    (testing "Streaming sources"
      (is
       (=
        '(fn
          [*depot]
          (let
           [*k (source> *depot)]
           (+compound $$p {*k (+count)})))
        (body->sexpr
         (transform-sexprs
          '(<<sources
            s
            (source> *depot :> *k)
            (+compound $$p {*k (+count)}))))))

      (is
       (=
        '(fn
          [*depot *depot-2]
          (let
           [[*k *v] (source> *depot {:retry-mode :individual})]
           (pr [*k *v])
           (+compound $$p {*k (+vec-agg *v)}))
          (let
           [*k (source> *depot-2)]
           (pr *k)
           (+compound $$p {*k (+count)})))
        (body->sexpr
         (transform-sexprs
          '(<<sources
            s
            (source> *depot {:retry-mode :individual} :> [*k *v])
            (pr [*k *v])
            (+compound $$p {*k (+vec-agg *v)})
            (source> *depot-2 :> *k)
            (pr *k)
            (+compound $$p {*k (+count)})))))))

    (testing "Microbatch sources"
      (is
       (=
        '(fn
          [*depot]
          (let
           [*k (source> *depot)]
           (do
             (inc *k)
             (let
              [*k (identity *k)]
              (+compound $$p {*k (+count)})))))
        (body->sexpr
         (transform-sexprs
          '(<<sources
            s
            (source> *depot :> *k)
            (inc *k :> *k)
            (+compound $$p {*k (+count)}))))))

      (is
       (=
        '(fn
          [*depot *depot-2]
          (let
           [%microbatch (source> *depot)]
           (let
            [[*k *v] (%microbatch)]
            (pr [*k *v])
            (+compound $$p {*k (+vec-agg *v)})))
          (let
           [%microbatch (source> *depot-2)]
           (let
            [*k (%microbatch)]
            (pr *k)
            (+compound $$p {*k (+count)}))))
        (body->sexpr
         (transform-sexprs
          '(<<sources
            s
            (source> *depot :> %microbatch)
            (%microbatch :> [*k *v])
            (pr [*k *v])
            (+compound $$p {*k (+vec-agg *v)})
            (source> *depot-2 :> %microbatch)
            (%microbatch :> *k)
            (pr *k)
            (+compound $$p {*k (+count)})))))))

    (with-testing-context
     "Errors"
     (transform-sexprs
      '(<<sources
        s
        (source> *depot-1 *depot-2 *depot-3 :> *k)
        (+compound $$p {*k (+count)})))

     (is (= err/syntax-error-source>-arity
            (-> utils/*ctx*
                :findings
                deref
                first
                :message))))))

(deftest <<subsource-test
  ;; Test <<subsource construct for tagged union type handling
  ;; Contract: transforms <<subsource into case with type-based dispatch

  (testing "<<subsource"
    (is
     (=
      '(case
        (type *data)
        (case> A)
        (do (let [{:keys [*a]} (case> A)] (prn *a)))
        (case> B)
        (do (let [{:keys [*b]} (case> B)] (prn *b))))
      (body->sexpr
       (transform-sexprs
        '(<<subsource
          *data
          (case> A :> {:keys [*a]})
          (prn *a)

          (case> B :> {:keys [*b]})
          (prn *b))))))

    (is
     (=
      '(let
        [*a nil
         _
         (case
          (type *data)
          (case> A)
          (do (let [{:keys [*a]} (case> A)] (prn *a {*a *a})))
          (case> B)
          (do (let [{:keys [*a]} (case> B)] (prn *a {*a *a}))))]
        (prn *a))
      (body->sexpr
       (transform-sexprs
        '(<<subsource
          *data
          (case> A :> {:keys [*a]})
          (prn *a)

          (case> B :> {:keys [*a]})
          (prn *a))
        '(prn *a)))))))
