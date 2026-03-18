(ns com.rpl.rama.util.helpers-hooks
    (:require [clj-kondo.hooks-api :as api]))

(defn- strip-schema-annotations
       "Removes `:- Type` pairs from a field vector's children.
  [a :- S b :- S] => [a b]"
       [children]
       (loop [cs     children
              result []]
             (if (seq cs)
                 (let [node (first cs)]
                      (if (and (api/keyword-node? node)
                               (= :- (api/sexpr node)))
                          ;; Skip :- and the following schema token
                          (recur (drop 2 cs) result)
                          (recur (rest cs) (conj result node))))
                 result)))

(defn defrecord+-hook
      "Transforms `defrecord+` into a standard `defrecord`.

  Strips schema annotations (`:- Type` pairs) from the field vector.
  (defrecord+ Foo [a :- Schema b :- Schema]) => (defrecord Foo [a b])"
      [{:keys [node]}]
      (let [[_ record-name fields & body] (:children node)
            plain-fields (strip-schema-annotations (:children fields))
            new-node (api/list-node
                      (list*
                       (api/token-node 'defrecord)
                       record-name
                       (api/vector-node plain-fields)
                       body))]
           {:node (with-meta new-node (meta node))}))
