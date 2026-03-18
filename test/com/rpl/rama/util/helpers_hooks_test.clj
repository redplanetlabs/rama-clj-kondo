(ns com.rpl.rama.util.helpers-hooks-test
    (:require
     [clojure.test :refer [deftest is testing]]
     [com.rpl.rama.util.helpers-hooks :as hooks]
     [com.rpl.test-helpers :refer [node->sexpr sexpr->node]]))

(deftest defrecord+-hook-test
  ;; defrecord+-hook strips :- Schema annotations and rewrites as defrecord
  (testing "defrecord+-hook"
           (testing "strips schema annotations"
                    (is (= '(defrecord Foo [a b c])
                           (node->sexpr
                            (hooks/defrecord+-hook
                             (sexpr->node
                              '(defrecord+ Foo [a :- Any b :- Any c :- Any])))))))

           (testing "handles fields without annotations"
                    (is (= '(defrecord Foo [a b])
                           (node->sexpr
                            (hooks/defrecord+-hook
                             (sexpr->node
                              '(defrecord+ Foo [a b])))))))

           (testing "handles empty fields"
                    (is (= '(defrecord Foo [])
                           (node->sexpr
                            (hooks/defrecord+-hook
                             (sexpr->node
                              '(defrecord+ Foo [])))))))))
