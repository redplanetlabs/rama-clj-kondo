(ns com.rpl.rama-hooks.special-forms-test
  (:require
   [clj-kondo.hooks-api :as api]
   [clojure.test :refer [deftest is testing]]
   [com.rpl.errors :as err]
   [com.rpl.rama-hooks :as rama]
   [com.rpl.test-helpers :refer [body->sexpr
                                  get-error-messages
                                  get-first-error-message
                                  node->sexpr
                                  sexpr->node
                                  transform-sexprs
                                  with-testing-context
                                  ->sexpr]]))

;;; Tests for special Rama forms that define new syntax or have special scoping rules.
;;; Covers: emit tokens, query topologies, batch/loop processing, ramaops, anchors, and aggregations.

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
