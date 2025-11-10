(ns com.rpl.rama-hooks.top-level-forms-test
  "Tests for top-level form transformations like defbasicsegmacro, 
   defbasicblocksegmacro, and top-level block hooks."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.rpl.rama-hooks :as rama]
   [com.rpl.test-helpers :refer [node->sexpr sexpr->node]]))

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
  ;; Tests transformation of deframaop and deframafn forms to defn.
  ;; Contracts: both forms support optional docstrings and transform correctly.
  (let [f (fn [form]
            (node->sexpr
             (rama/deframaop-hook
              (sexpr->node form))))]
    (testing "deframaop without docstring"
      (is (= '(defn f [] (:> nil)) (f '(deframaop f [] (:>))))))
    
    (testing "deframaop with docstring"
      (is (= '(defn f "A function" [] (:> nil))
             (f '(deframaop f "A function" [] (:>))))))
    
    (testing "deframafn without docstring"
      (is (= '(defn f [*a] (:> nil)) (f '(deframafn f [*a] (:>))))))
    
    (testing "deframafn with docstring"
      (is (= '(defn f "Takes a value" [*a] (:> nil))
             (f '(deframafn f "Takes a value" [*a] (:>))))))
    
    (testing "deframaop with multi-line docstring"
      (is (= '(defn f "Line 1\nLine 2" [] (:> nil))
             (f '(deframaop f "Line 1\nLine 2" [] (:>))))))))
