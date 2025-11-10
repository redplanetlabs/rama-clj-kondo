(ns com.rpl.rama-hooks-test
  (:require
   [clj-kondo.hooks-api :as api]
   [clj-kondo.impl.utils :as utils :refer [parse-string]]
   [clojure.test :refer [deftest is testing]]
   [com.rpl.errors :as err]
   [com.rpl.rama-hooks :as rama]
   [com.rpl.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-first-error-message
  []
  (-> utils/*ctx*
      :findings
      deref
      first
      :message))

(defn get-error-messages
  []
  (->> utils/*ctx*
       :findings
       deref
       (mapv :message)))

(defmacro with-testing-context
  [test-desc & body]
  `(testing ~test-desc
     (binding [utils/*ctx* {:base-lang  :clj
                            :lang       :clj
                            :namespaces (atom {:clj {:clj {}}})
                            :findings   (atom [])
                            :ignores    (atom {})}]
       ~@body)))

(defn ->sexpr
  [expr]
  (parse-string (prn-str expr)))

(defn transform-sexprs
  [& sexprs]
  (let [sexprs (mapv ->sexpr sexprs)]
    (rama/transform-body sexprs)))

(defn transform-module-sexprs
  [& sexprs]
  (rama/transform-module-body
   (mapv ->sexpr sexprs)
   #{}))

(defn strip-trampoline
  [sexpr]
  (rama/remove-instances sexpr #{'trampoline}))

(defn sexpr->node
  [x]
  {:node (->sexpr x)})

(defn node->sexpr
  [x]
  (strip-trampoline (api/sexpr (:node x))))

(defn body->sexpr
  [x]
  (strip-trampoline (api/sexpr (first x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest strip-trampoline-test
  (is (= '[] (strip-trampoline '[trampoline])))
  (is (= '[[[]]] (strip-trampoline '[trampoline [trampoline [trampoline]]])))
  (is (= '[([])] (strip-trampoline '[trampoline (trampoline [trampoline])])))
  (is (= '[([])] (strip-trampoline '[trampoline (trampoline [trampoline])])))
  (is (= '[pr [+ 1 2] 3]
         (strip-trampoline '[trampoline pr [trampoline + 1 2] 3])))

  (is (= '(let [*one (identity 1)] (pr *one))
         (strip-trampoline
          (api/sexpr
           (first
            (transform-sexprs
             '(identity 1 :> *one)
             '(pr *one)))))))

  (is (= '(let
           [{:keys [*one *two]} (hash-map :one 1 :two 2)]
           (pr *one))
         (strip-trampoline
          (api/sexpr
           (first
            (transform-sexprs
             '(hash-map :one 1 :two 2 :> {:keys [*one *two]})
             '(pr *one))))))))

(deftest rama-trampolined-test
  (testing "Rama var becomes a let"
    (is (= '(let [*one (trampoline identity 1)] (trampoline pr *one))
           (api/sexpr
            (first
             (transform-sexprs
              '(identity 1 :> *one)
              '(pr *one)))))))

  (testing "Destructuring binds"
    (is
     (= '(let
          [{:keys [*one *two]} (trampoline hash-map :one 1 :two 2)]
          (trampoline pr *one))
        (api/sexpr
         (first
          (transform-sexprs
           '(hash-map :one 1 :two 2 :> {:keys [*one *two]})
           '(pr *one)))))))

  (testing "Identity reassignment"
    (is
     (=
      '(let
        [*one (trampoline identity 1)]
        (do
         (trampoline identity 2)
         (let
          [*one (identity *one)]
          (trampoline pr *one))))
      (api/sexpr
       (first
        (transform-sexprs
         '(identity 1 :> *one)
         '(identity 2 :> *one)
         '(pr *one))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic functionality tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest rama-test
  (testing "Rama var becomes a let"
    (is (= '(let [*one (identity 1)] (pr *one))
           (body->sexpr
            (transform-sexprs
             '(identity 1 :> *one)
             '(pr *one))))))

  (testing "Various collection types"
    (is
     (= '(let [*a (identity :a)]
           (filter> (contains? #{:x :y :z}) [:a :b :c])
           (identity (into {:a 1 :b 2} [[:c 3] [:d 4]])))
        (body->sexpr
         (transform-sexprs
          '(identity :a :> *a)
          '(filter> (contains? #{:x :y :z}) [:a :b :c])
          '(identity (into {:a 1 :b 2} [[:c 3] [:d 4]])))))))

  (testing "Destructuring binds"
    (is
     (= '(let
          [{:keys [*one *two]} (hash-map :one 1 :two 2)]
          (pr *one))
        (body->sexpr
         (transform-sexprs
          '(hash-map :one 1 :two 2 :> {:keys [*one *two]})
          '(pr *one))))))

  (testing "Multiple assignments"
    (is
     (=
      '(let
        [*one (identity 1)]
        (let
         [*two (identity 2)]
         (let
          [*three (identity 3)]
          (pr [*one *two *three]))))
      (body->sexpr
       (transform-sexprs
        '(identity 1 :> *one)
        '(identity 2 :> *two)
        '(identity 3 :> *three)
        '(pr [*one *two *three]))))))

  (testing "Identity reassignment"
    (is
     (=
      '(let
        [*one (identity 1)]
        (do
         (identity 2)
         (let
          [*one (identity *one)]
          (pr *one))))
      (body->sexpr
       (transform-sexprs
        '(identity 1 :> *one)
        '(identity 2 :> *one)
        '(pr *one))))))

  (with-testing-context "Err"
    (binding [rama/*context* :dataflow]
      (is (= '(let [*map (identity {:a 1 :b 2})]
                (let [*a (:a *map)]
                  (when *success?
                    (letfn
                        [(%deduct [*curr] (:> (- *curr *amt)))]
                        (local-transform>
                         [(keypath *from-user-id)
                          (term %deduct)]
                         $$funds)))))
             (body->sexpr
              (transform-sexprs
               '(identity {:a 1 :b 2} :> *map)
               '(:a *map :> *a)
               '(<<if
                 *success?
                 (<<ramafn
                  %deduct [*curr]
                  (:> (- *curr *amt)))
                 (local-transform>
                  [(keypath *from-user-id) (term %deduct)] $$funds)))))))

    (is (= err/syntax-error-keyword-fn-in-dataflow
          (-> utils/*ctx*
              :findings
              deref
              first
              :message)))))

(deftest multiple-outputs
  (is (= '(let [[*one *two] (identity 1)] (pr *one))
         (body->sexpr
          (transform-sexprs
           '(identity 1 :> *one *two)
           '(pr *one)))))
  
  (is (= '(let [[*one *two *three] (identity 1)] (pr *one))
         (body->sexpr
          (transform-sexprs
           '(identity 1 :> *one *two :other> *three)
           '(pr *one)))))
  
  (is (= '(let [[*three *one *two] (identity 1)] (pr *one))
         (body->sexpr
          (transform-sexprs
           '(identity 1 :other> *three :> *one *two)
           '(pr *one)))))
  
  (is (= '(let [<other> nil
                [*three *one *two] (identity 1)]
            (pr *one))
         (body->sexpr
          (transform-sexprs
           '(identity 1 :other> <other> *three :> *one *two)
           '(pr *one)))))
  
  (is (= '(let [<other> nil
                <test> nil
                [*three *one *two] (identity 1)]
            (pr *one))
         (body->sexpr
          (transform-sexprs
           '(identity 1 :other> <other> *three :> <test> *one *two)
           '(pr *one))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top level forms tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest defbasicsegmacro-test
  (is
   (=
    '(defn traverse>
       [path target out-var]
       (let [out-var (or out-var (gen-anyvar))]
         [sc/traverse>* target (seg# path> path) :> out-var]))
    (node->sexpr
     (rama/defsegmacro-hook
       (sexpr->node
        '(defbasicsegmacro
           traverse>
           [path target :> out-var]
           (let [out-var (or out-var (gen-anyvar))]
             [sc/traverse>* target (seg# path> path) :> out-var]))))))))

(deftest defbasicblocksegmacro-test
  (is
   (=
    '(defn
      my-sm
      [arg1 arg2 output-arg another-stream-arg]
      [[+ arg1 arg2 :> '*v#]
       [* '*v# output-arg :> another-stream-arg]])
    (node->sexpr
     (rama/defsegmacro-hook
      (sexpr->node
       '(defbasicblocksegmacro
         my-sm
         [arg1 arg2 :> output-arg :a> another-stream-arg]
         [[+ arg1 arg2 :> '*v#]
          [* '*v# output-arg :> another-stream-arg]])))))))

(deftest top-level-block-hook-test
  (let [result '(do (let [*x (identity 1)]))]
    (is (= result
           (node->sexpr
            (rama/top-level-block-hook
             (sexpr->node '(?<- (identity 1 :> *x)))))))
    (is (= result
           (node->sexpr
            (rama/top-level-block-hook
             (sexpr->node '(<<do (identity 1 :> *x)))))))
    (is (= result
           (node->sexpr
            (rama/top-level-block-hook
             (sexpr->node '(<<branch (identity 1 :> *x)))))))
    (is (= result
           (node->sexpr
            (rama/top-level-block-hook
             (sexpr->node '(<<atomic (identity 1 :> *x)))))))))

(deftest defn-like-hook-test
  (let [f (fn [form]
            (node->sexpr
             (rama/deframaop-hook
              (sexpr->node form))))]
    (is (= '(defn f [] (:> nil)) (f '(deframaop f [] (:>)))))
    (is (= '(defn f [*a] (:> nil)) (f '(deframafn f [*a] (:>)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Branching tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest <<if-test
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
              :message)))))

(deftest <<switch-test
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
              :message)))))

(deftest <<cond-test
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
           (three)))))))))

(deftest <<sources-test
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
              :message)))

  ))

(deftest <<subsource-test
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
        (prn *b)
       )))))

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
      '(prn *a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special form tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest emit-test
  (is (= '(:> nil)
         (body->sexpr
          (transform-sexprs '(:>))))))

(deftest <<query-topology-test
  (is
   (=
    '(fn
      name
      [*a *b *c]
      (let
       [*ret (+ *b *c)]
       [*ret]))
    (body->sexpr
     (rama/transform-module-form
      (->sexpr
       '(<<query-topology
         x
         "name"
         [*a *b *c :> *ret]
         (+ *b *c :> *ret)))
      nil
      #{}))))

  (is
   (=
    '(fn
       name
       []
       (let
           [*ret (identity :x)]
           [*ret]))
    (body->sexpr
     (rama/transform-module-form
      (->sexpr
       '(<<query-topology
         x
         "name"
         [:> *ret]
         (identity :x :> *ret)))
      nil
      #{})))
   "0 arity query topology")

  (is
   (=
    '(fn
       name
       [*url *start-bucket *end-bucket]
       (|hash *url)
       (let
           [[*granularity *gstart *gend]
            (explode (query-granularities :m *start-bucket *end-bucket))]
           (let
               [*bucket-stat
                (local-select>
                 [(keypath *url *granularity)
                  (sorted-map-range *gstart *gend) MAP-VALS]
                 $$window-stats)]
               (|origin)
               (let
                   [*stats (+combine-measurements *bucket-stat)]
                   [*stats]))))
    (-> '(<<query-topology
          topologies
          "name"
          [*url *start-bucket *end-bucket :> *stats]
          (|hash *url)
          (explode
           (query-granularities :m *start-bucket *end-bucket)
           :> [*granularity *gstart *gend])
          (local-select>
           [(keypath *url *granularity)
            (sorted-map-range *gstart *gend)
            MAP-VALS]
           $$window-stats
           :> *bucket-stat)
          (|origin)
          (+combine-measurements *bucket-stat :> *stats))
        ->sexpr
        (rama/transform-module-form nil #{})
        body->sexpr))))

(deftest batch<--test
  (is
   (=
    '(fn [] (let [] (let [*x (identity 1)] [])))
    (node->sexpr
     (rama/batch<--hook
      (sexpr->node
       '(batch<-
         []
         (identity 1 :> *x)))))))

  (is
   (=
    '(fn
        []
        (let [$$user-total-spend "$$user-total-spend"]
          (do (microbatch)
              (let
                  [*purchase-cents (microbatch)
                   *user-id (identity *user-id)]
                (|hash *user-id)
                (let [[*total-spend-cents] []]
                  (pr $$user-total-spend
                      {*user-id (do (+sum *purchase-cents)
                                    (let [*total-spend-cents (identity *total-spend-cents)]))})
                  [*user-id *total-spend-cents])))))

    (node->sexpr
     (rama/batch<--hook
      (sexpr->node
       '(batch<-
         [*user-id *total-spend-cents]
         (microbatch :> {:keys [*user-id *purchase-cents]})
         (|hash *user-id)
         (+compound
          $$user-total-spend
          {*user-id (+sum
                     *purchase-cents
                     :new-val> *total-spend-cents)}))))))))

(deftest loop<--test
  (is
   (=
    '(let
      [[!ret]
       (let
        [!res 0]
        (if
         (pos? !res)
         (:> !res)
         (continue> (+ !res 1))))]
      (pr !ret))
    (body->sexpr
     (transform-sexprs
      '(loop<-
        [!res 0 :> !ret]
        (<<if
         (pos? !res)
         (:> !res)
        (else>)
         (continue> (+ !res 1))))
      '(pr !ret))))))

(deftest <<ramaop-test
  (testing "Making sure that ramaop works in no context"
    (is
     (=
      '(letfn
        [(%op
          [*a *b]
          (let
           [*c (trampoline + *a *b)]
           (:> *c)))])
      (api/sexpr
       (first
        (transform-sexprs
         '(<<ramaop
           %op
           [*a *b]
           (+ *a *b :> *c)
           (:> *c))))))))

  (testing "Testing that it correctly defines something in a greater scope"
    (is
     (=
      '(letfn
        [(%f
          [*a *b]
          (let
           [*c (trampoline + *a *b)]
           (:> *c)))]
        (let
         [*sum (trampoline %f 1 2)]
         (trampoline pr *sum)))
      (api/sexpr
       (first
        (transform-sexprs
         '(<<ramaop
           %f
           [*a *b]
           (+ *a *b :> *c)
           (:> *c))
         '(%f 1 2 :> *sum)
         '(pr *sum)))))))

  (is
   (=
    '(letfn
      [(%deduct
        [*curr]
        (:> (trampoline - *curr *amt)))]
      (trampoline
       local-transform>
       [(trampoline keypath *from-user-id) (trampoline term %deduct)]
       $$funds))
    (api/sexpr
     (first
      (transform-sexprs
       '(<<ramafn
         %deduct
         [*curr]
         (:> (- *curr *amt)))
       '(local-transform>
         [(keypath *from-user-id) (term %deduct)]
         $$funds))))))

  (with-testing-context
   "Missing ramaop name"
   (transform-sexprs
    '(<<ramaop
      [*curr]
      (:> (- *curr *amt))))

   (is (= err/syntax-error-missing-def-name (get-first-error-message)))
   (is (= 1 (count (get-error-messages)))))

  (with-testing-context
   "Invalid ramaop name"
   (transform-sexprs
    '(<<ramaop
      *deduct
      [*curr]
      (:> (- *curr *amt))))

   (is (= err/syntax-error-invalid-ramaop-name (get-first-error-message)))
   (is (= 1 (count (get-error-messages)))))

  (with-testing-context
   "Missing input vector"
   (transform-sexprs
    '(<<ramaop
      %deduct
      (:> (- *curr *amt))))

   (is (= err/syntax-error-missing-input-vector (get-first-error-message)))
   (is (= 1 (count (get-error-messages)))))
)

(deftest depot-partition-append-test
  (is (=
       '(let
         [*depot nil]
         (depot-partition-append! *depot 1 :ack))
       (body->sexpr
        (transform-sexprs
         '(depot-partition-append! *depot 1 :ack))))))

;; TODO: open ticket:
;; (anchor> <X>)
;; (<<branch <X>
;;           (identity 1 :> *x)
;;           (anchor> <Y>))
;; (<<branch <Y>)
;; This should work.
;; The concept of ramavars and branches probably need to be split up so that
;; the branches can be forwarded back outside the scope in which they were
;; defined
(deftest anchor>-test
  (is
   (=
    '(let
      [<X> nil]
      (<<branch
       <X>
       (let [*x (identity 1)])))
    (body->sexpr
     (transform-sexprs
      '(anchor> <X>)
      '(<<branch
        <X>
        (identity 1 :> *x)))))))

(deftest <<do-test
  (is
   (= '(do
        (let
         [*x (identity 1)]
         (prn *x)))
      (body->sexpr
       (transform-sexprs
        '(<<do (identity 1 :> *x))
        '(prn *x))))))

(deftest +group-by
  (is
   (=
    '(+group-by
      !k
      (let
       [!cs (+combine-extra-sum [1] !v)]
       (let
        [!as (+accum-extra-sum [!n] !v)]
        (pr !k !cs !as))))
    (body->sexpr
     (transform-sexprs
      '(+group-by
        !k
        (+combine-extra-sum [1] !v :> !cs)
        (+accum-extra-sum [!n] !v :> !as))
      '(pr !k !cs !as)))
   ))

  (is
   (=
    '(do
      (<<batch
       (let
        [!v (range> 1 6)]
        (let
         [!k (mod !v 2)]
         (+group-by
          !k
          (let
           [!avg (+avg !v)]
           (let
            [!count (+count)]
            (v-swap! captured conj {!k [!avg !count]}))))))))
    (node->sexpr
     (rama/top-level-block-hook
      (sexpr->node
       '(?<-
         (<<batch
          (range> 1 6 :> !v)
          (mod !v 2 :> !k)
          (+group-by
           !k
           (+avg !v :> !avg)
           (+count :> !count))
          (v-swap! captured conj {!k [!avg !count]}))))))
   )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module code tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest module-hook-test
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
          (get-error-messages)))))

(deftest defmodule-hook-test
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
          (get-error-messages)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure interop tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Moved to test/com/rpl/rama_hooks/interop_test.clj

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java interop tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Moved to test/com/rpl/rama_hooks/interop_test.clj

;; Moved to test/com/rpl/rama_hooks/interop_test.clj

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Illegal contexts tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Moved to test/com/rpl/rama_hooks/validation_test.clj
