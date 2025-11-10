(ns com.rpl.rama-hooks.interop-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.rpl.test-helpers :refer [body->sexpr transform-sexprs]]))

;;; Test interactions between Rama dataflow and Clojure/Java code.
;;; Covers clj! for escaping to Clojure evaluation, java-block<- for
;;; dataflow within Java macros, and java-macro! for string-based
;;; variable passing to Java interop.

(deftest clj!-test
  (testing "clj!"
    (testing "preserves nested Clojure code unchanged"
      (is (= '(clj! (let [m {:a 1 :b 2}]
                      (prn (:a m))))
             (body->sexpr
              (transform-sexprs
               '(clj! (let [m {:a 1 :b 2}]
                        (prn (:a m)))))))))

    (testing "allows referencing dataflow variables"
      (is (= '(let [*a (identity 1)]
                (let [*b (identity 2)]
                  (let [*c (+ 3 (clj!
                                 (let [m {:a *a :b *b}]
                                   (reduce (fn [a v] (+ a v)) (vals m)))))]
                    (prn *c))
                  ))
             (body->sexpr
              (transform-sexprs
               '(identity 1 :> *a)
               '(identity 2 :> *b)
               '(+ 3 (clj! (let [m {:a *a :b *b}]
                             (reduce (fn [a v] (+ a v)) (vals m))))
                   :> *c)
               '(prn *c)
               )))))))

(deftest java-block<--test
  (testing "java-block<-"
    (testing "enables dataflow inside java-macro! context"
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
          '(pr "*aVar"))))))))

(deftest java-macro!-test
  (testing "java-macro!"
    (testing "converts variable references to strings"
      (is
       (= '(let
            [_ (pr) [*user-id] []]
            (pr [*user-id])
            (.genid id-gen "*user-id"))
          (body->sexpr
           (transform-sexprs
            '(java-macro! (.genid id-gen "*user-id")))))))

    (testing "handles multiple variable types"
      (is
       (= '(let
            [_ (pr) [*a !b %c] []]
            (pr [*a !b %c])
            (.genid id-gen "*a" "!b" "%c"))
          (body->sexpr
           (transform-sexprs
            '(java-macro! (.genid id-gen "*a" "!b" "%c")))))))

    (testing "handles invalid variable names"
      (is (= '(let [_ (pr) [] []] (pr []) (.genid id-gen "*a b"))
             (body->sexpr
              (transform-sexprs
               '(java-macro! (.genid id-gen "*a b")))))))

    (testing "preserves regular strings"
      (is (= '(let [_ (pr) [] []] (pr []) (.genid id-gen "something"))
             (body->sexpr
              (transform-sexprs '(java-macro! (.genid id-gen "something")))))))

    (testing "integrates with dataflow transformations"
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
          '(pr *c))))))))
