(ns com.rpl.test-helpers
    "Shared test utilities for rama-clj-kondo tests."
    (:require
     [clj-kondo.hooks-api :as api]
     [clj-kondo.impl.utils :as utils :refer [parse-string]]
     [clojure.test :refer [testing]]
     [com.rpl.rama-hooks :as rama]))

(defn get-first-error-message
      "Returns the message from the first finding in the current test context."
      []
      (-> utils/*ctx*
          :findings
          deref
          first
          :message))

(defn get-error-messages
      "Returns a vector of all finding messages in the current test context."
      []
      (->> utils/*ctx*
           :findings
           deref
           (mapv :message)))

(defmacro with-testing-context
          "Creates a testing context with isolated clj-kondo state for findings.
  Use this to wrap test assertions that check for linting errors."
          [test-desc & body]
          `(testing ~test-desc
                    (binding [utils/*ctx* {:base-lang  :clj
                                           :lang       :clj
                                           :namespaces (atom {:clj {:clj {}}})
                                           :findings   (atom [])
                                           :ignores    (atom {})}]
                             ~@body)))

(defn ->sexpr
      "Converts a Clojure expression to a clj-kondo node by printing and parsing."
      [expr]
      (parse-string (prn-str expr)))

(defn transform-sexprs
      "Transforms multiple s-expressions using rama/transform-body."
      [& sexprs]
      (let [sexprs (mapv ->sexpr sexprs)]
           (rama/transform-body sexprs)))

(defn transform-module-sexprs
      "Transforms multiple s-expressions as module body using rama/transform-module-body."
      [& sexprs]
      (rama/transform-module-body
       (mapv ->sexpr sexprs)
       #{}))

(defn strip-trampoline
      "Removes all instances of 'trampoline symbols from the s-expression."
      [sexpr]
      (rama/remove-instances sexpr #{'trampoline}))

(defn sexpr->node
      "Wraps an s-expression in a node map for testing."
      [x]
      {:node (->sexpr x)})

(defn node->sexpr
      "Extracts the s-expression from a node map, stripping trampoline symbols."
      [x]
      (strip-trampoline (api/sexpr (:node x))))

(defn body->sexpr
      "Extracts the s-expression from the first item in a sequence, stripping trampoline symbols."
      [x]
      (strip-trampoline (api/sexpr (first x))))
