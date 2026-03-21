(ns com.rpl.agent-o-rama.hooks
    "clj-kondo hooks for agent-o-rama macros.
  TODO: move these hooks to the agent-o-rama repo."
    (:require [clj-kondo.hooks-api :as api]))

(defn- strip-schema-annotations
       "Strips :- Schema pairs from a defrecord+ field vector,
  returning only the field name nodes."
       [field-nodes]
       (loop [nodes field-nodes
              fields []]
             (if (empty? nodes)
                 fields
                 (let [node (first nodes)]
                      (if (and (api/keyword-node? node)
                               (= :- (:k node)))
          ;; Skip :- and the schema expression that follows
                          (recur (drop 2 nodes) fields)
                          (recur (rest nodes) (conj fields node)))))))

(defn defaorrecord-hook
      "Transforms defaorrecord into defrecord for clj-kondo analysis.

  (defaorrecord Name
    [field1 :- Schema1
     field2 :- Schema2]
    Protocol
    (method [this] body))

  becomes:

  (defrecord Name [field1 field2]
    Protocol
    (method [this] body))"
      [{:keys [node]}]
      (let [[_ name-node fields-node & rest-nodes] (:children node)
            field-names (strip-schema-annotations (:children fields-node))
            new-node (api/list-node
                      (into [(api/token-node 'defrecord)
                             name-node
                             (api/vector-node field-names)]
                            rest-nodes))]
           {:node (with-meta new-node (meta node))}))

(defn defmetric-hook
      "Transforms defmetric into def for clj-kondo analysis.

  (defmetric Name {...}) becomes (def Name (do {...}))"
      [{:keys [node]}]
      (let [[_ name-node & body] (:children node)
            new-node (api/list-node
                      (list* (api/token-node 'def)
                             name-node
                             body))]
           {:node (with-meta new-node (meta node))}))
