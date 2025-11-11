(ns com.rpl.rama-hooks.basic-test
    (:require
     [clj-kondo.hooks-api :as api]
     [clojure.test :refer [deftest is testing]]
     [com.rpl.errors :as err]
     [com.rpl.rama-hooks :as rama]
     [com.rpl.test-helpers :refer [body->sexpr
                                   strip-trampoline
                                   transform-sexprs
                                   with-testing-context]]))

;;; Helpers tests

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

;;; Basic functionality tests

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
           (-> clj-kondo.impl.utils/*ctx*
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
