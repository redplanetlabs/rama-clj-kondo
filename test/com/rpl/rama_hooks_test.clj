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

  (testing "Destructuring binds"
    (is
     (= '(let
          [{:keys [*one *two]} (hash-map :one 1 :two 2)]
          (pr *one))
        (body->sexpr
         (transform-sexprs
          '(hash-map :one 1 :two 2 :> {:keys [*one *two]})
          '(pr *one))))))

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
         [[*k *v] (source> *depot)]
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
         (source> *depot :> [*k *v])
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
     (source> *depot-1 *depot-2 :> *k)
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
      (do (let [{:keys [*a]} (case> A)] (println *a)))
     (case> B)
      (do (let [{:keys [*b]} (case> B)] (println *b))))
    (body->sexpr
     (transform-sexprs
      '(<<subsource
        *data
       (case> A :> {:keys [*a]})
        (println *a)

       (case> B :> {:keys [*b]})
        (println *b)
       )))))

  (is
   (=
    '(let
      [*a nil
       _
       (case
        (type *data)
       (case> A)
        (do (let [{:keys [*a]} (case> A)] (println *a {*a *a})))
       (case> B)
        (do (let [{:keys [*a]} (case> B)] (println *a {*a *a}))))]
      (println *a))
    (body->sexpr
     (transform-sexprs
      '(<<subsource
        *data
       (case> A :> {:keys [*a]})
        (println *a)

       (case> B :> {:keys [*a]})
        (println *a))
      '(println *a))))))

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
      (let
       [$$user-total-spend "$$user-total-spend"]
       (do
        (microbatch)
        (let
         [*purchase-cents (microbatch)
          *user-id (identity *user-id)]
         (|hash *user-id)
         (let
          [[*total-spend-cents] []]
          (pr
           $$user-total-spend
           {*user-id (+sum
                      *purchase-cents
                      :new-val> *total-spend-cents)})
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
         (println *x)))
      (body->sexpr
       (transform-sexprs
        '(<<do (identity 1 :> *x))
        '(println *x))))))

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
;;; Java interop tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest java-block<--test
  (is
   (=
    '(let
      [_ (pr) [*aVar] []]
      (pr [*aVar])
      (.aMacro
       "*aVar"
       (java-block<-
        (let
         [*bVar (identity 1)]
         (let [*c (+ *aVar *bVar)]))))
      (pr "*aVar")
     )
    (body->sexpr
     (transform-sexprs
      '(java-macro!
        (.aMacro
         "*aVar"
         (java-block<-
          (identity 1 :> *bVar)
          (+ *aVar *bVar :> *c))))
      '(pr "*aVar"))))))

(deftest java-macro!-test
  (is
   (= '(let
        [_ (pr) [*user-id] []]
        (pr [*user-id])
        (.genid id-gen "*user-id"))
      (body->sexpr
       (transform-sexprs
        '(java-macro! (.genid id-gen "*user-id"))))))

  (is
   (= '(let
        [_ (pr) [*a !b %c] []]
        (pr [*a !b %c])
        (.genid id-gen "*a" "!b" "%c"))
      (body->sexpr
       (transform-sexprs
        '(java-macro! (.genid id-gen "*a" "!b" "%c"))))))

  (is (= '(let [_ (pr) [] []] (pr []) (.genid id-gen "*a b"))
         (body->sexpr
          (transform-sexprs
           '(java-macro! (.genid id-gen "*a b"))))))

  (is (= '(let [_ (pr) [] []] (pr []) (.genid id-gen "something"))
         (body->sexpr
          (transform-sexprs '(java-macro! (.genid id-gen "something"))))))

  (is
   (=
    '(let
      [*a (identity 1)]
      (let
       [*b (identity 2)]
       (let
        [_ (pr *b *a) [*c] []]
        (pr [*c])
        (.sum "*a" "*b" "*c")
        (pr *c)
       )))
    (body->sexpr
     (transform-sexprs
      '(identity 1 :> *a)
      '(identity 2 :> *b)
      '(java-macro!
        (.sum "*a" "*b" "*c"))
      '(pr *c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rama demo gallery example tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def bank-transfer-module-code
  '(defmodule
    BankTransferModule
    [setup topologies]
    (declare-depot setup *transfer-depot (hash-by :from-user-id))
    (declare-depot setup *deposit-depot (hash-by :user-id))
    (let
     [mb (microbatch-topology topologies "banking")]
     (declare-pstate mb $$funds {Long Long})
     (declare-pstate
      mb
      $$outgoing-transfers
      {Long (map-schema
             String                 ; transfer-id
             (fixed-keys-schema
              {:to-user-id Long
               :amt        Long
               :success?   Boolean})
             {:subindex? true})})
     (declare-pstate
      mb
      $$incoming-transfers
      {Long (map-schema
             String                 ; transfer-id
             (fixed-keys-schema
              {:from-user-id Long
               :amt          Long
               :success?     Boolean})
             {:subindex? true})})
     (<<sources
      mb
     (source> *transfer-depot :> %microbatch)
      (%microbatch :> {:keys [*transfer-id *from-user-id *to-user-id *amt]})
      (local-select> [(keypath *from-user-id) (nil->val 0)] $$funds :> *funds)
      (>= *funds *amt :> *success?)
      (<<if
       *success?
       (<<ramafn
        %deduct
        [*curr]
        (:> (- *curr *amt)))
       (local-transform> [(keypath *from-user-id) (term %deduct)] $$funds))
      (local-transform>
       [(keypath *from-user-id *transfer-id)
        (termval
         {:to-user-id *to-user-id
          :amt        *amt
          :success?   *success?})]
       $$outgoing-transfers)
      (|hash *to-user-id)
      (<<if
       *success?
       (+compound $$funds {*to-user-id (aggs/+sum *amt)}))
      (local-transform>
       [(keypath *to-user-id *transfer-id)
        (termval
         {:from-user-id *from-user-id
          :amt          *amt
          :success?     *success?})]
       $$incoming-transfers)
     (source> *deposit-depot :> %microbatch)
      (%microbatch :> {:keys [*user-id *amt]})
      (+compound $$funds {*user-id (aggs/+sum *amt)})))))

(def bank-transfer-module-transformed-code
  '(defn
    BankTransferModule
    [setup topologies]
    (let
     [*transfer-depot (declare-depot setup (hash-by :from-user-id))]
     (pr *transfer-depot)
     (let
      [*deposit-depot (declare-depot setup (hash-by :user-id))]
      (pr *deposit-depot)
      (let
       [mb (microbatch-topology topologies "banking")]
       (let
        [$$funds (declare-pstate mb {Long Long})]
        (pr $$funds)
        (let
         [$$outgoing-transfers
          (declare-pstate
           mb
           {Long (map-schema
                  String ; transfer-id
                  (fixed-keys-schema
                   {:to-user-id Long
                    :amt        Long
                    :success?   Boolean})
                  {:subindex? true})})]
         (pr $$outgoing-transfers)
         (let
          [$$incoming-transfers
           (declare-pstate
            mb
            {Long (map-schema
                   String     ; transfer-id
                   (fixed-keys-schema
                    {:from-user-id Long
                     :amt          Long
                     :success?     Boolean})
                   {:subindex? true})})]
          (pr $$incoming-transfers)
          (fn
           []
           (let
            [%microbatch (source> *transfer-depot)]
            (let
             [{:keys [*transfer-id *from-user-id *to-user-id *amt]}
              (%microbatch)]
             (let
              [*funds
               (local-select>
                [(keypath *from-user-id) (nil->val 0)]
                $$funds)]
              (let
               [*success? (>= *funds *amt)]
               (when
                *success?
                (letfn
                 [(%deduct [*curr] (:> (- *curr *amt)))]
                 (local-transform>
                  [(keypath *from-user-id)
                   (term %deduct)]
                  $$funds)))

               (local-transform>
                [(keypath *from-user-id *transfer-id)
                 (termval
                  {:to-user-id *to-user-id
                   :amt        *amt
                   :success?   *success?})]
                $$outgoing-transfers)
               (|hash *to-user-id)
               (when
                *success?
                (+compound
                 $$funds
                 {*to-user-id (aggs/+sum *amt)}))
               (local-transform>
                [(keypath *to-user-id *transfer-id)
                 (termval
                  {:from-user-id *from-user-id
                   :amt          *amt
                   :success?     *success?})]
                $$incoming-transfers)))))
           (let
            [%microbatch (source> *deposit-depot)]
            (let
             [{:keys [*user-id *amt]} (%microbatch)]
             (+compound
              $$funds
              {*user-id (aggs/+sum *amt)}))))))))))))

(deftest demo-gallery-bank-transfer-module-test
  (is (= bank-transfer-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node bank-transfer-module-code))))))

(def profile-gallery-module-code
  '(defmodule
    ProfileModule
    [setup topologies]
    (declare-depot setup *registration-depot (hash-by :username))
    (declare-depot setup *profile-edits-depot (hash-by :user-id))
    (let
     [s (stream-topology topologies "profiles")
      id-gen (ModuleUniqueIdPState. "$$id")]
     (declare-pstate
      s
      $$username->registration
      {String ; username
       (fixed-keys-schema
        {:user-id Long
         :uuid    String})})
     (declare-pstate
      s
      $$profiles
      {Long ; user ID
       (fixed-keys-schema
        {:username      String
         :pwd-hash      String
         :display-name  String
         :height-inches Long})})
     (.declarePState id-gen s)
     (<<sources
      s
     (source> *registration-depot :> {:keys [*uuid *username *pwd-hash]})
      (local-select>
       (keypath *username)
       $$username->registration
       :>
       {*curr-uuid :uuid :as *curr-info})
      (<<if
       (or>
        (nil? *curr-info)
        (= *curr-uuid *uuid))
       (java-macro! (.genId id-gen "*user-id"))
       (local-transform>
        [(keypath *username)
         (multi-path
          [:user-id (termval *user-id)]
          [:uuid (termval *uuid)])]
        $$username->registration)
       (|hash *user-id)
       (local-transform>
        [(keypath *user-id)
         (multi-path
          [:username (termval *username)]
          [:pwd-hash (termval *pwd-hash)])]
        $$profiles))
     (source> *profile-edits-depot :> {:keys [*user-id *edits]})
      (explode *edits :> {:keys [*field *value]})
      (local-transform>
       [(keypath *user-id *field) (termval *value)]
       $$profiles)))))

(def profile-gallery-module-transformed-code
  '(defn
    ProfileModule
    [setup topologies]
    (let
     [*registration-depot (declare-depot setup (hash-by :username))]
     (pr *registration-depot)
     (let
      [*profile-edits-depot (declare-depot setup (hash-by :user-id))]
      (pr *profile-edits-depot)
      (let
       [s (stream-topology topologies "profiles")
        id-gen (ModuleUniqueIdPState. "$$id")]
       (let
        [$$username->registration
         (declare-pstate
          s
          {String
           (fixed-keys-schema
            {:user-id Long
             :uuid    String})})]
        (pr $$username->registration)
        (let
         [$$profiles
          (declare-pstate
           s
           {Long (fixed-keys-schema
                  {:username      String
                   :pwd-hash      String
                   :display-name  String
                   :height-inches Long})})]
         (pr $$profiles)
         (.declarePState id-gen s)
         (fn
          []
          (let
           [{:keys [*uuid *username *pwd-hash]}
            (source> *registration-depot)]
           (let
            [{:as *curr-info *curr-uuid :uuid}
             (local-select>
              (keypath *username)
              $$username->registration)]
            (when
             (or>
              (nil? *curr-info)
              (= *curr-uuid *uuid))
             (let
              [_ (pr) [*user-id] []]
              (pr [*user-id])
              (.genId id-gen "*user-id")
              (local-transform>
               [(keypath *username)
                (multi-path
                 [:user-id (termval *user-id)]
                 [:uuid (termval *uuid)])]
               $$username->registration)
              (|hash *user-id)
              (local-transform>
               [(keypath *user-id)
                (multi-path
                 [:username (termval *username)]
                 [:pwd-hash (termval *pwd-hash)])]
               $$profiles)))))
          (let
           [{:keys [*user-id *edits]}
            (source> *profile-edits-depot)]
           (let
            [{:keys [*field *value]} (explode *edits)]
            (local-transform>
             [(keypath *user-id *field) (termval *value)]
             $$profiles)))))))))))

(deftest demo-gallery-profile-module-test
  (is (= profile-gallery-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node profile-gallery-module-code))))))

(def time-series-module-code
  '(defmodule
    TimeSeriesModule
    [setup topologies]
    (declare-depot setup *render-latency-depot (hash-by :url))
    (let
     [mb (microbatch-topology topologies "timeseries")]
     (declare-pstate
      mb
      $$window-stats
      {String                ; url
       {clojure.lang.Keyword ; granularity
        (map-schema
         Long                ; bucket
         WindowStats
         {:subindex? true})}})
     (<<sources
      mb
     (source> *render-latency-depot :> %microbatch)
      (%microbatch :> {:keys [*url *render-millis *timestamp-millis]})
      (single-window-stat *render-millis :> *single-stat)
      (emit-index-granularities *timestamp-millis :> *granularity *bucket)
      (+compound
       $$window-stats
       {*url
        {*granularity
         {*bucket (+combine-measurements *single-stat)}}}))
     (<<query-topology
      topologies
      "get-stats-for-minute-range"
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
      (+combine-measurements *bucket-stat :> *stats)))))

(def time-series-module-transformed-code
  '(defn
    TimeSeriesModule
    [setup topologies]
    (let
     [*render-latency-depot (declare-depot setup (hash-by :url))]
     (pr *render-latency-depot)
     (let
      [mb (microbatch-topology topologies "timeseries")]
      (let
       [$$window-stats
        (declare-pstate
         mb
         {String              ; url
          {clojure.lang.Keyword ; granularity
           (map-schema
            Long              ; bucket
            WindowStats
            {:subindex? true})}})]
       (pr $$window-stats)
       (fn
        []
        (let
         [%microbatch (source> *render-latency-depot)]
         (let
          [{:keys [*url *render-millis *timestamp-millis]} (%microbatch)]
          (let
           [*single-stat (single-window-stat *render-millis)]
           (let
            [[*granularity *bucket]
             (emit-index-granularities *timestamp-millis)]
            (+compound
             $$window-stats
             {*url
              {*granularity
               {*bucket (+combine-measurements
                         *single-stat)}}}))))))
       (fn
        get-stats-for-minute-range
        [*url *start-bucket *end-bucket]
        (|hash *url)
        (let
         [[*granularity *gstart *gend]
          (explode
           (query-granularities
            :m
            *start-bucket
            *end-bucket))]
         (let
          [*bucket-stat
           (local-select>
            [(keypath *url *granularity)
             (sorted-map-range *gstart *gend)
             MAP-VALS]
            $$window-stats)]
          (|origin)
          (let
           [*stats (+combine-measurements *bucket-stat)]
           [*stats])))))))))

(deftest demo-gallery-time-seties-module-test
  (is (= time-series-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node time-series-module-code))))))

(def top-users-module-code
  '(defmodule
    TopUsersModule
    [setup topologies]
    (declare-depot setup *purchase-depot (hash-by :user-id))
    (let
     [mb (microbatch-topology topologies "topusers")]
     (declare-pstate mb $$user-total-spend {Long Long})
     (declare-pstate mb $$top-spending-users java.util.List {:global? true})
     (<<sources
      mb
     (source> *purchase-depot :> %microbatch)
      (<<batch
       (<<if
        true
        (identity 1 :> *x)
       (else>)
        (identity 2 :> *x))
       (pr *x)
       (loop<-
        [*x [1 2 3] :> *ret])
       (user-spend-subbatch %microbatch :> *user-id *total-spend-cents)
       (vector *user-id *total-spend-cents :> *tuple)
       (|global)
       (+top-monotonic
        [TOP-AMOUNT]
        $$top-spending-users
        *tuple
        :+options
        {:id-fn       first
         :sort-val-fn last}))))))

(def top-users-module-transformed-code
  '(defn
    TopUsersModule
    [setup topologies]
    (let
     [*purchase-depot (declare-depot setup (hash-by :user-id))]
     (pr *purchase-depot)
     (let
      [mb (microbatch-topology topologies "topusers")]
      (let
       [$$user-total-spend (declare-pstate mb {Long Long})]
       (pr $$user-total-spend)
       (let
        [$$top-spending-users
         (declare-pstate mb java.util.List {:global? true})]
        (pr $$top-spending-users)
        (fn
         []
         (let
          [%microbatch (source> *purchase-depot)]
          (<<batch
           (let
            [*x nil
             _
             (if
              true
              (let [*x (identity 1)] {*x *x})
              (let [*x (identity 2)] {*x *x}))]
            (pr *x)
            (let
             [[*ret] (let [*x [1 2 3]])]
             (let
              [[*user-id *total-spend-cents]
               (user-spend-subbatch %microbatch)]
              (let
               [*tuple (vector *user-id *total-spend-cents)]
               (|global)
               (+top-monotonic
                [TOP-AMOUNT]
                $$top-spending-users
                *tuple
                :+options
                {:id-fn       first
                 :sort-val-fn last}))))))))))))))

(deftest demo-gallery-top-users-module-test
  (is (= top-users-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node top-users-module-code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Illegal contexts tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            '(when true? (println "true"))))

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
