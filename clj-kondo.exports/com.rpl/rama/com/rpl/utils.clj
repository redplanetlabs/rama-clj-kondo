(ns com.rpl.utils
  (:require
   [clojure.pprint :as pprint]
   [clj-kondo.hooks-api :as api]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For debugging...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prnm
  "Debug print, with metadata.

  Printing metadata can be very important for dealing with clj-kondo hooks
  because it contains information about the source location. So sometimes when
  the metadata is wrong, errors will appear on the incorrect line in an editor,
  so printing the metadata can be very helpful."
  [& x]
  (binding [*print-meta* true]
    (apply prn x)))

(defn sppy
  "Printing a form will only tell you the token type of the top level form.
  Pretty printing a whole s-expr will give you the full nested records, which
  is helpful for debugging things like a missing call to api/token-node to wrap
  a symbol."
  ([x] (sppy "SPY:" x))
  ([p x]
   (print p " ")
   (pprint/pprint x)
   x))

(defn spy
  ([x] (spy "SPY:" x))
  ([p x]
   (prn p x)
   x))

(defn rama-contains?
  "given a set of symbols, check if the given token matches any of them.

  if the token is a prefixed symbol, we make sure that the namespace is a rama
  namespace, and the set contains the simple symbol."
  [symbol-set token]
  (when token
    (let [symbol (api/sexpr token)]
      (if (qualified-symbol? symbol)
        (let [{:keys [name ns]} (api/resolve {:name symbol})]
          (and
           (= ns 'com.rpl.rama)
           (contains? symbol-set name)))
        (contains? symbol-set symbol)))))

(defn rama=
  [a b]
  (rama-contains? #{a} b))
