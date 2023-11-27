(ns com.rpl.errors
  (:require
   [clj-kondo.hooks-api :as api]
   [clojure.string :as str]
   [com.rpl.utils :as u]))

(defn mk-finding
  [message metadata]
  {:message message
   :type    :rama-syntax-error
   :level   :error
   :row     (:row metadata)
   :col     (:col metadata)
   :end-row (:row metadata)
   :end-col (:col metadata)})

(def syntax-error-case>-arity
  "(case> condition) takes exactly 1 argument")
(def syntax-error-default>-arity
  "(default> ...) takes 0 or 2 arguments")
(def syntax-error-else>-arity
  "(else>) takes no arguments")
(def syntax-error-source>-arity
  "(source> ...) takes exactly 1 argument")
(def syntax-error-subsource-case>-arity
  "(case> ...) takes exactly 1 argument, plus an optional emit")
(def syntax-error-multiple-elses
  "<<if cannot have more than one `(else>)`")
(def syntax-error-missing-input-vector
  "missing input vector")
(def syntax-error-missing-def-name
  "def form missing name")
(def syntax-error-missing-topology-name
  "missing topology name")
(def syntax-error-missing-pobject-name
  "missing pobject name")
(def syntax-error-invalid-pobject-name
  "pobject name must begin with `*`")
(def syntax-error-invalid-pstate-name
  "pstate name must begin with `$$`")
(def syntax-error-invalid-ramaop-name
  "<<ramaop or <<ramafn name must begin with `%`")
(def syntax-error-source>-first
  "<<sources block must begin with `(source> ...)`")

(defn error!
  [message metadata]
  (api/reg-finding!
   (mk-finding message metadata)))

(defn maybe-missing-def-name
  [maybe-name-node metadata]
  (when-not (and (api/token-node? maybe-name-node)
                 (symbol? (:value maybe-name-node)))
    (error! syntax-error-missing-def-name metadata)))

(defn maybe-invalid-ramaop-name
  [name-node metadata]
  (when-not (and (api/token-node? name-node)
                 (= \% (first (str (:value name-node)))))
    (error! syntax-error-invalid-ramaop-name metadata)))

(defn maybe-missing-input-vector
  [maybe-input-node metadata]
  (when-not (api/vector-node? maybe-input-node)
    (error! syntax-error-missing-input-vector metadata)))

(defn maybe-case-arity
  [case-node metadata]
  (when (not= 2 (count (:children case-node)))
    (error! syntax-error-case>-arity metadata)))

(defn maybe-default-arity
  [default-node metadata]
  (when-not (#{1 3} (count (:children default-node)))
    (error! syntax-error-default>-arity metadata)))

(defn maybe-source-arity
  [source-expr metadata]
  (when (not= 2 (count source-expr))
    (error! syntax-error-source>-arity metadata)))

(defn maybe-subsource-case-arity
  [case-expr metadata]
  (when-not (or (= 2 (count case-expr))
                (= 4 (count case-expr)))
    (error! syntax-error-subsource-case>-arity metadata)))

(defn maybe-else-arity
  [else-node metadata]
  (when-let [[else-marker] else-node]
    (when (not= 1 (count (:children else-marker)))
      (error! syntax-error-else>-arity metadata))))

(defn maybe-multiple-elses
  [maybe-extra-nodes metadata]
  (when (seq maybe-extra-nodes)
    (error! syntax-error-multiple-elses metadata)))

(defn maybe-missing-topology-name
  [maybe-topology-name metadata]
  (when-not (and (api/token-node? maybe-topology-name)
                 (symbol? (:value maybe-topology-name)))
    (error! syntax-error-missing-topology-name metadata)))

(defn maybe-missing-pobject-name
  [maybe-pobject-name object-type metadata]
  (let [pobject-name     (:value maybe-pobject-name)
        pobject-name-str (str pobject-name)]
    (if (and (api/token-node? maybe-pobject-name)
             (symbol? pobject-name))
      (cond
        (and (u/rama-contains? '#{declare-pstate} (:value object-type))
             (not (str/starts-with? pobject-name-str "$$")))
        (error! syntax-error-invalid-pstate-name metadata)
        (not (contains? #{\* \$} (first pobject-name-str)))
        (error! syntax-error-invalid-pobject-name metadata))
      (error! syntax-error-missing-pobject-name metadata))))

(defn maybe-source>-first
  [[[block]] metadata]
  (if (api/list-node? block)
    (let [[token] (:children block)]
      (when-not (and (api/token-node? token)
                     (u/rama= 'source> (:value token)))
        (error! syntax-error-source>-first metadata)))
    (error! syntax-error-source>-first metadata)))
