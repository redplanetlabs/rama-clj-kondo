(ns com.rpl.rama-hooks
    "clj-kondo hooks for rama code."
    (:require
     [clj-kondo.hooks-api :as api]
     [clojure.pprint]
     [clojure.set :as set]
     [clojure.string :as str]
     [clojure.walk :as walk]
     [com.rpl.errors :as err]
     [com.rpl.utils :as u :refer [rama= rama-contains?]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn flatten-all
      "clojure.core/flatten doesn't work on maps, so this one does."
      [x]
      (filter (complement coll?)
              (rest (tree-seq coll? seq x))))

(defn node?
      "clj-kondo has an `api/node?` function, but it's not available in older
  versions of clj-kondo, so this is just to let this tool work with slightly
  older versions as well rather than requiring the latest."
      [x]
      ((some-fn api/keyword-node?
                api/list-node?
                api/map-node?
                api/set-node?
                api/string-node?
                api/token-node?
                api/vector-node?)
       x))

(defn emit-node?
      [node]
      (when (api/keyword-node? node)
            (let [k (str (:k node))]
                 (and (str/ends-with? k ">")
                      (not (some #{\space \newline} k))))))

(defn pstate-or-nil
      "Returns the token if it represents a pstate. Otherwise, return `nil`"
      [token]
      (when (and (symbol? token)
                 (str/starts-with? (str token) "$$"))
            token))

(defn ramavar-or-nil
      "Given a string, return the ramavar it represents.
  If it's not a ramavar, return `nil`"
      [token]
      (when
       (and (string? token)
            (contains? #{\* \! \% \$} (first token))
            (not (some #{\space \newline} token)))
       (symbol token)))

(defn separate-new-and-old-bindings
      "Given a set of known existing bindings, and a set of potentially new ones,
  split those potentially new ones into ones that do already exist, and ones
  that don't.

  Ex. (separate-new-and-old-bindings '#{*a *b} '#{*b *c *d})
  => [#{*b} #{*c *d}]
  "
      [existing maybe-new]
      [(set/difference maybe-new existing)
       (set/intersection existing maybe-new)])

(defn find-all-pstates
      "Given a form, find all pstates referenced within it."
      [body]
      (->>
       (mapv api/sexpr body)
       (flatten)
       (into #{} (comp (map pstate-or-nil) (remove nil?)))))

(defn find-all-ramavars
      "Given an expression, flatten the whole thing and return a set of every
  ramavar referenced within it."
      [expr]
      (when expr
            (let [expr     (api/sexpr (if (node? expr) [expr] expr))
                  metadata (meta expr)]
                 (with-meta
                  (into #{}
                        (comp (map ramavar-or-nil)
                              (remove nil?)
                              (map #(with-meta % metadata)))
                        (flatten-all expr))
                  metadata))))

(defn remove-instances
      "Walks a tree and strips all instances of any symbol within the given set
  from the tree."
      [sexpr symbol-set]
      (walk/postwalk
       (fn [x]
           (cond
            (list? x)
            (concat (empty x) (remove #(contains? symbol-set %) x))
            (map-entry? x)
            x
            (coll? x)
            (into (empty x) (remove #(contains? symbol-set %) x))
            :else x))
       sexpr))

(defn- wrap-form
       "Wraps body in a form with the given token, or returns the single node unwrapped."
       [token body]
       (if (= 1 (count body))
           (first body)
           (api/list-node
            (list*
             (api/token-node token)
             body))))

(defn- let-node
       "Build a (let [bindings...] body...) AST node."
       [bindings body]
       (api/list-node
        (list*
         (api/token-node 'let)
         (api/vector-node bindings)
         body)))

(defn- inject-ramavars-map
       [ramavars body]
       (let [fval (:value (second (:children (first body))))]
            (if (or (::case-marker (meta (first body)))
                    (rama-contains? '#{case> default>} fval))
                (first body)
                (wrap-form 'do
                           (concat
                            (butlast body)
                            (if (api/list-node? (last (:children (last body))))
                                [(api/list-node
                                  (concat
                                   (butlast (:children (last body)))
                                   [(inject-ramavars-map ramavars
                                                         [(last (:children (last body)))])]))]
                                [(api/list-node
                                  (concat
                                   (:children (last body))
                                   [(api/map-node
                                     (mapcat
                                      (fn [ramavar]
                                          [(api/token-node ramavar)
                                           (api/token-node ramavar)])
                                      ramavars))]))]))))))

(defn eliminate-unchecked-branches
      "Remove expressions that shouldn't be taken into consideration when
  attempting to unify branches.
    - Any `case>` nodes should be discarded
    - Any `default>` nodes should be discarded
    - A body nodes who's preceeding `default>` node contains `:unify false`"
      [branches bodies]
      (->> (map vector branches bodies)
       ;; Remove any bodies who's preceeding `default>` node is `:unify false`
           (partition-all 2)
           (remove
            (fn [[[[branch]]]]
                (and (rama= 'default> (:value (first (:children branch))))
                     (= '(:unify false) (rest (api/sexpr branch))))))
           (mapcat identity)
       ;; Remove any `case>`, `default>`, and case-marker nodes
           (remove
            (fn [[[branch]]]
                (or (::case-marker (meta branch))
                    (rama-contains? '#{case> default>}
                                    (:value (first (:children branch)))))))
           (mapv (comp ::ramavars meta second))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: There are certain contexts in which some forms cannot be used.
;; For example, throughout Rama's dataflow code, Clojure primitive forms
;; or macros that expand to them cannot be used.
;; We want to keep track of the current contexts that forms are being used
;; in to make sure that illegal usage isn't allowed.

(def illegal-forms
     '#{declare def defonce defn defn- definline defmacro quote var
        memfn let letfn set!
        and or
        when when-not when-let when-first when-some
        if if-not if-let if-some
        cond condp case
        loop recur doseq dotimes for while
        binding locking time with-in-str with-out-str with-precision
        with-local-vars with-open with-redefs with-redefs-fn
        do try catch finally throw
        as-> cond-> cond->> some-> some->> .. doto
        defprotocol extend-type extend-protocol
        defrecord deftype defmethod defmulti
        definterface proxy
        ns import
        vswap! monitor-enter monitor-exit})

(defn java-method?
      [form]
      (let [form-str (str form)]
    ;; NOTE: clojure reserves symbols starting or ending with `.`, so we know
    ;; that this will have to be a Java method or constructor call.
           (or (str/starts-with? form-str ".")
               (str/ends-with? form-str "."))))

(defn keyword-fn?
      [form]
      (let [form-str (str form)]
           (and (str/starts-with? form-str ":")
         ;; If it ends with ">" then it's an emit token
         ;; ex. :> or :err>
                (not (str/ends-with? form-str ">")))))

(def ^:dynamic *context* nil)

(defmulti ^:private validate-form
          (fn [form]
              (let [value (or (:value form) (:k form) form)]
                   (cond
                    (keyword-fn? value) :keyword-fn-form
                    (= 'fn value) :lambda-fn-form
                    (contains? illegal-forms value) :special-form
                    (java-method? value) :java-form
                    :else form))))

(defmethod validate-form :default
           [_form]
           true)

(defmethod validate-form :keyword-fn-form
           [form]
           (err/maybe-illegal-keyword-fn *context* (meta form)))

(defmethod validate-form :lambda-fn-form
           [form]
           (err/maybe-illegal-lambda *context* (meta form)))

(defmethod validate-form :special-form
           [form]
           (err/maybe-illegal-special-form *context* (:value form) (meta form)))

(defmethod validate-form :java-form
           [form]
           (err/maybe-illegal-java-form *context* (meta form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transformers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: while normally it might make more sense to declare the handling for
;; all of these special forms directly inside the clj-kondo config, that
;; doesn't really work for Rama code.
;; Having access to the following forms is very important, because we need to
;; be able to wrap the following forms into the `let`s generated by these
;; transformers. As such, the clj-kondo config only defines transformers for
;; the "top level" forms - ie any form that can contain Rama code, or other
;; things that require special transformations (such as `defmodule`, which
;; doesn't contain Rama code itself, but follows somewhat similar rules)

;; We forward declare these vars to support mutual recursion between individual
;; transformers and these forms that do the heavy lifting.
(declare transform-body)
(declare transform-module-body)

(defn- is-anchor-node?
       "Checks if a node represents a Rama anchor (e.g., <my-anchor>)."
       [node]
       (when (api/token-node? node)
             (let [v (:value node)]
                  (and (symbol? v)
                       (let [s (str v)]
                            (and (> (count s) 2) ; Must be at least < >
                                 (str/starts-with? s "<")
                                 (str/ends-with? s ">")))))))

;; This is kinda the most important part of his this works
;; It splits an expression into 2 parts, using the emit `:>` to split on.
;; The right hand side of that will get bound in a `let`, with the left hand
;; side being the value.
;; Ex:
;; (identity "Hello world" :> *hello)
;; will return
;; [(<token: identity> <token: "Hello world">)
;;  <token: *hello>
;;  [*hello]
;;  []
;;  #{*hello}]
;; This is used later on in `transform-form` to rewrite this as
;; (let [*hello (identity "Hello World")] ...)
(defn- extract-emits
       "Separate the expression, output variables, new bindings, and rebinds from a segment."
       [all-terms curr-ramavars]
       (if (emit-node? (first all-terms)) ;; handle (:> *arg1) differently from (operation :> *out1)
           [all-terms
            []
            nil
            nil
            #{}
            []]
           (loop [terms all-terms
                  expr-nodes []
                  all-output-var-nodes []
                  found-anchor-nodes []
                  state :inputs]
                 (let [current-term (first terms)
                       remaining    (rest terms)]
                      (cond
          ;; End of terms, process collected data
                       (empty? terms)
                       (let [;; Calculate final vars based on collected output-vars
                             ramavars (find-all-ramavars all-output-var-nodes) ; Get symbols
                             new-vars (set/difference ramavars curr-ramavars)
                             rebinds  (set/intersection ramavars curr-ramavars)]
                            [expr-nodes ; Expression part (list of nodes, op is the first one)
                             all-output-var-nodes ; List of all output variable *nodes*
                             (when (seq new-vars) (into [] new-vars)) ; List of new variable *symbols*
                             (when (seq rebinds) (into [] rebinds)) ; List of rebound variable *symbols*
                             ramavars ; Set of all output variable *symbols*
                             found-anchor-nodes])

          ;; If we find an emit keyword while expecting inputs, switch state
                       (and (= state :inputs) (emit-node? current-term))
                       (recur remaining expr-nodes all-output-var-nodes found-anchor-nodes :output-body)

          ;; State: capture input arguments (including the initial operation)
                       (= state :inputs)
                       (recur remaining (conj expr-nodes current-term) all-output-var-nodes found-anchor-nodes :inputs)

          ;; If we find another emit keyword while processing an output body,
          ;; just skip it and stay in output-body state for the next term.
                       (and (= state :output-body) (emit-node? current-term))
                       (recur remaining expr-nodes all-output-var-nodes found-anchor-nodes :output-body)

          ;; State: process the body of an output stream declaration
                       (= state :output-body)
                       (if (is-anchor-node? current-term)
            ;; It's an anchor, collect it
                           (recur remaining expr-nodes all-output-var-nodes (conj found-anchor-nodes current-term) :output-body)
            ;; It's not an anchor, assume it's a variable node
                           (recur remaining expr-nodes (conj all-output-var-nodes current-term) found-anchor-nodes :output-body))

          ;; Fallback/Error case (shouldn't ideally happen with correct input)
                       :else
                       (throw (ex-info "Unexpected state in extract-emits" {:state state :term current-term})))))))

;; This is handling the special case in query topologies where the input
;; ramavars might be empty, but there's still emit vars. Something such as:
;;   (<<query-topology
;;     topologies
;;     "name"
;;     [:> *ret]
;;     (identity :x :> *ret))
;; Without this special handling, and just calling `extract-emits` directly,
;; it will mistakenly try using `:> *ret` as a binding form, which can cause
;; confusing linting issues.
(defn- extract-binding-emits
       "Separate the input vars from output vars in something like a query topology
  definition or loop<- bindings"
       [terms curr-ramavars]
       (if (and (api/keyword-node? (first terms))
                (= :> (:k (first terms))))
           [[] (rest terms)]
           (extract-emits terms curr-ramavars)))

;; NOTE: the following implementations of `split-form` are for any form that
;; provides multiple branches. Ex. `<<if`, `<<cond`, `<<switch`, etc. These
;; then return multiple branches that will have their emit vars checked. This
;; is to facilitate the unification of the emit vars on these branches.
;; For example, given
;;    (<<if true
;;      (identity 1 :> *x)
;;     (else>)
;;      (identity 2 :> *x))
;;    (println *x)
;; we want here the *x on each branch of the `<<if` to be captured so that it's
;; note just available in the scope of each branch, but rather after the `<<if`
;; altogether.
;; The `split-form` facilitates this by returning each branch that will then
;; get transformed individually, while also allowing the emits to be looked at
;; to be unified.

(defmulti ^:private split-form
          "Partition children into distinct blocks"
          (fn [_f children _ramavars]
              (let [block-type (:value (first children))]
                   (if (qualified-symbol? block-type)
                       (:name (api/resolve {:name block-type}))
                       block-type))))

(defmethod split-form :default
           [_f _children _ramavars]
           {:prefix nil :branches nil})

;; (<<sources s
;;   (source> *depot :> [*k *v])
;;   (println *k *v)
;;   (source> *depot-2 :> *k)
;;   (println *k))
;; becomes
;; (fn [*depot *depot-2]
;;   (let [[*k *v] (identity *depot)]
;;     (println *k *v))
;;   (let [*k (identity *depot-2)]
;;     (println *k)))
(defmethod split-form '<<sources
           [f children ramavars]
           (let [metadata (meta f)
                 [_<<sources topology & _sources] children
                 missing-topology? (err/maybe-missing-topology-name topology metadata)
                 blocks   (partition-by #(and (api/list-node? %)
                                              (rama= 'source>
                                                     (:value (first (:children %)))))
                                        (if missing-topology?
                                            (rest children)
                                            (drop 2 children)))
                 source-not-first? (err/maybe-source>-first blocks metadata)
                 blocks   (if source-not-first? (rest blocks) blocks)
                 branches (partition-all 2 blocks)]
                {:prefix   [(api/token-node 'fn)
                ;; Get a set of all the depot names that we don't know of an
                ;; existing binding for that symbol. That way we'll simply bind
                ;; the depot name in the input vector for the function, but for
                ;; depots that we *do* know exist, we can still lint them
                ;; properly with their source.
                            (as-> branches $
                                  (map (fn [[[source]]] (:value (second (:children source)))) $)
                                  (into #{} $)
                                  (set/difference $ ramavars)
                                  (map api/token-node $)
                                  (api/vector-node $))]
                 :branches
                 (mapv (fn [[[source] [& block]]]
                           (let [[input] (extract-emits (:children source) ramavars)]
                                (err/maybe-source-arity input (meta source)))
                           (concat [source] block))
                       branches)}))

(defmethod split-form '<<subsource
           [f children _ramavars]
           (let [metadata (meta f)
                 [_<<subsource data & sources] children
                 blocks   (partition-by #(and (api/list-node? %)
                                              (rama= 'case>
                                                     (:value (first (:children %)))))
                                        sources)
                 branches (partition-all 2 blocks)]
                {:prefix   [(api/token-node 'case)
                            (api/list-node
                             (list (api/token-node 'type) data))]
                 :branches
                 (mapcat
                  (fn [[[marker] body]]
                      (let [[_case> type-node] (:children marker)
                            case-const (vary-meta type-node assoc ::case-marker true)]
                           (err/maybe-subsource-case-arity (:children marker) metadata)
                           [[case-const] [(wrap-form '<<do (concat [marker] body))]]))
                  branches)}))

(defmethod split-form '<<switch
           [f children _ramavars]
           (let [m      (meta f)
                 blocks (->> (drop 2 children)
                             (partition-by
                              #(and (api/list-node? %)
                                    (rama= 'case> (:value (first (:children %))))))
                             (partition-all 2))]
                {:prefix   [(api/token-node 'cond)]
                 :branches
                 (mapcat
                  (fn [[[marker] body]]
                      (err/maybe-case-arity marker m)
        ;; TODO: make this use `(second children)`
        ;; Having this as a `case` doesn't work.
                      [[(api/list-node (list (api/token-node 'case>)
                                             (api/list-node
                                              (list*
                                               (api/token-node '=)
                                               (second children)
                                               (rest (:children marker))))))]
                       [(wrap-form '<<do body)]])
                  blocks)}))

(defmethod split-form '<<cond
           [f children _ramavars]
           (let [m      (meta f)
                 blocks (->> (rest children)
                             (partition-by
                              #(and (api/list-node? %)
                                    (rama-contains? '#{case> default>}
                                                    (:value (first (:children %))))))
                             (partition-all 2))]
                {:prefix   [(api/token-node 'cond)]
                 :branches
                 (mapcat (fn [[[marker] body]]
                             (if (rama= 'case> (:value (first (:children marker))))
                                 (err/maybe-case-arity marker m)
                                 (err/maybe-default-arity marker m))
                             [[marker] [(wrap-form '<<do body)]])
                         blocks)}))

(defmethod split-form '<<if
           [f children _ramavars]
           (let [m (meta f)
                 [if-block else-marker else-block & extra]
                 (partition-by
                  #(and (api/list-node? %)
                        (rama= 'else> (:value (first (:children %)))))
                  (drop 2 children))]
                (err/maybe-else-arity else-marker m)
                (err/maybe-multiple-elses extra m)

                {:prefix   [(api/token-node (if else-marker 'if 'when))
                            (second children)]
                 :branches
                 (if else-marker
                     [[(wrap-form '<<do if-block)] [(wrap-form '<<do else-block)]]
                     [if-block])}))

;; NOTE: the following forms, like the ones above, are handled specially. They
;; either define new syntax, or have certain extra rules to them, such as
;; forcing clj-kondo to ignore if referenced variables don't exist, or making
;; sure that the variables they define get referenced.
;; For example, `<<ramaop` forms need to be completely rewritten such that they
;; define a `letfn` with the op inside, whereas `depot-partition-append` can
;; be permitted to reference depots that there's no real way to show that they
;; exist in the given scope.

(defmulti ^:private handle-form
          "Partition children into distinct blocks"
          (fn [node _following _ramavars]
              (if (emit-node? (first (:children node)))
                  :emit
                  (let [value (:value (first (:children node)))]
                       (if (qualified-symbol? value)
                           (:name (api/resolve {:name value}))
                           value)))))

(defmethod handle-form :default
           [_node _following _ramavars]
           nil)

(defmethod handle-form :emit
           [node following _ramavars]
           (if (= 1 (count (:children node)))
               [(with-meta (api/list-node (list (first (:children node))
                                                (api/token-node nil)))
                           (meta node))
                following]
               nil))

;; (java-macro!
;;  (.aMacro "*aVar" (java-block<-
;;                    (identity 1 :> *bVar)
;;                    (+ *aVar *bVar :> *c))))
;; (pr "*aVar")
;;
;; becomes...
;; (let [_ (pr)
;;       [*aVar] []]
;;   (pr [*aVar])
;;   (.aMacro "*aVar"
;;            (java-block<-
;;             (let [*bVar (identity 1)]
;;               (let [*c (+ *aVar *bVar)]))))
;;   (pr "*aVar"))
(defmethod handle-form 'java-macro!
           [node following ramavars]
           (let [[_java-macro!-token expr] (:children node)
                 found-vars   (into #{}
                                    (comp
                                     (map ramavar-or-nil)
                                     (remove nil?))
                                    (api/sexpr expr))
                 [new old]    (separate-new-and-old-bindings ramavars found-vars)
                 new-bindings (api/vector-node (map api/token-node new))
                 use-ramavars (api/list-node (list
                                              (api/token-node 'pr)
                                              new-bindings))
        ;; NOTE: We're inserting a trampoline here because when we transform
        ;; the body, we don't want the method call symbol to be the first
        ;; argument. This will cause a linting error since it appears as if the
        ;; java interop is happening from inside dataflow code (which it is, but
        ;; normally that's disallowed, so we want to allow it here)
                 linted-expr  (api/list-node (list* 'trampoline (:children expr)))
                 new-node     (let-node
                               [(api/token-node '_)
                                (api/list-node
                                 (list* (api/token-node 'pr) (map api/token-node old)))
                                new-bindings
                                (api/vector-node [])]
                               (transform-body
                                (concat [use-ramavars] [linted-expr] following)
                                ramavars))
                 metadata     (meta node)]
                [(with-meta new-node metadata) nil]))

;; (+compound
;;     $$user-total-spend
;;     {*user-id (+sum *purchase-cents
;;                     :new-val> *total-spend-cents)})
;;
;; becomes...
;; (let [[*total-spend-cents] []]
;;   (pr $$user-total-spend
;;       {*user-id (+sum *purchase-cents
;;                       :new-val> *total-spend-cents)}))
(defmethod handle-form '+compound
           [node following ramavars]
           (let [outvars (->> (api/sexpr node)
                              flatten-all
                              (partition-by #(= :new-val> %))
                              rest
                              (partition 2)
                              (map (comp first second)))]
                (if (seq outvars)
                    (let [[_ $$pstate template] (:children node)
                          fake-node (api/list-node (list (api/token-node 'pr)
                                                         $$pstate
                                                         template))]
                         [(with-meta
                           (let-node
                            [(api/vector-node (map api/token-node outvars))
                             (api/vector-node [])]
                            (transform-body (cons fake-node following) ramavars))
                           (meta node))
                          nil])
                    [node following])))

;; (batch<- [*y]
;;   (identity 1 :> *x)
;;   (* *x 2 :> *y))
;;
;; becomes...
;; (fn []
;;   (let []
;;     (let [*x (identity 1)]
;;       (let [*y (* *x 2)]
;;         [*y]))))
(defmethod handle-form 'batch<-
           [node following ramavars]
           (let [[_ input & body] (:children node)
                 ramavars         (set/union ramavars (find-all-ramavars input))
                 pstates          (find-all-pstates body)
        ;; This form will output something like
        ;; (fn [...input]
        ;;   (let [...pstates ...(str pstate)]]
        ;;     ...body))
        ;; This makes sure that all pstates referenced in the block are bound
        ;; to variables, even if they're non-existant pstates.
                 new-node
                 (api/list-node
                  (list
                   (api/token-node 'fn)
                   (api/vector-node [])
                   (let-node
                    (mapcat
                     #(vector (api/token-node %)
                              (api/string-node (str %)))
                     pstates)
                    (transform-body (concat body [input]) ramavars))))
                 metadata         (meta node)]
                [(with-meta new-node metadata) following]))

;; (<<query-topology x
;;   "name"
;;   [*a *b *c :> *ret]
;;   (+ *b *c :> *ret))
;;
;; becomes...
;; (fn name [*a *b *c]
;;   (let [*ret (+ *b *c)]
;;     [*ret]))
(defmethod handle-form '<<query-topology
           [node following ramavars]
           (let [metadata (meta node)
                 [_ _topology name input-output & body] (:children node)
                 [input new-bindings]
                 (extract-binding-emits (:children input-output) #{})
                 ret-node (with-meta (api/vector-node new-bindings)
                                     metadata)
                 new-node (with-meta
                           (api/list-node
                            (into [(api/token-node 'fn)
                                   (api/token-node (symbol (api/sexpr name)))
                                   (api/vector-node input)]
                                  (transform-body (concat body [ret-node]) ramavars)))
                           metadata)]
                [new-node following]))

;; (<<ramaop %op
;;   [*a *b]
;;   (+ *a *b :> *c)
;;   (:> *c))
;; (%op 1 2)
;;
;; becomes...
;; (letfn
;;   [(%op [*a *b]
;;         (let [*c (trampoline + *a *b)]
;;           (:> *c)))]
;;   (%op 1 2))
(defn handle-frags
      "Transform a Rama <<ramaop or <<ramafn

  Turns the form into a letfn.

  Called by `transform-form`, NOT in the clj-kondo config because in that
  context we don't have access to the following nodes that are requires to
  embed in the resulting form to mimic the lexical scoping"
      [node & [following]]
      (let [metadata (meta node)
            [_ name input & body] (:children node)
            new-node
            (api/list-node
             (list*
              (api/token-node 'letfn)
              (api/vector-node
               [(api/list-node
                 (list*
                  name
                  input
                  (transform-body body)))])
              (transform-body following)))]
           (when-not (err/maybe-missing-def-name name metadata)
                     (err/maybe-invalid-ramaop-name name metadata)
                     (err/maybe-missing-input-vector input metadata))
           [(with-meta new-node metadata) nil]))

(defmethod handle-form '<<ramaop
           [node following ramavars]
           (handle-frags node following ramavars))

(defmethod handle-form '<<ramafn
           [node following ramavars]
           (handle-frags node following ramavars))

;; (loop<- [!res 0 :> !ret]
;;   (<<if (pos? !res)
;;     (:> !res)
;;    (else>)
;;     (continue> (+ !res 1))))
;;
;; becomes...
;; (let [[!ret]
;;       (let [!res 0]
;;        (if (pos? !res)
;;         (:> !res)
;;         (continue> (+ !res 1))))]
;;   (pr !ret))
(defn handle-loops
      [node following ramavars]
      (let [metadata         (meta node)
            [_loop-token decls & body] (:children node)
            [inputs outputs] (extract-emits (:children decls) ramavars)
            ;; NOTE: it would be more semantic to turn this into a `loop`
            ;; form, but then we'd get "loop without recur" warnings.
            new-node
            (let-node
             [(api/vector-node outputs)
              (let-node inputs (transform-body body ramavars))]
             (transform-body following ramavars))]
           [(with-meta new-node metadata) nil]))

(defmethod handle-form 'loop<-
           [node following ramavars]
           (handle-loops node following ramavars))

(defmethod handle-form 'async-loop<-
           [node following ramavars]
           (handle-loops node following ramavars))

(defmethod handle-form 'depot-partition-append!
           [node following ramavars]
           (let [depot-name (:value (second (:children node)))]
    ;; NOTE: We can perform a `depot-partition-append!` in a context where the
    ;; depot's symbol isn't bound. If that's the case, we're just going to wrap
    ;; the call in a new context where the depot is defined.
                (if (rama-contains? ramavars depot-name)
                    [node following]
                    (let [metadata (meta node)
                          new-node (let-node
                                    [(api/token-node depot-name)
                                     (api/token-node nil)]
                                    (transform-body (concat [node] following)
                                                    (conj ramavars depot-name)))]
                         [(with-meta new-node metadata) nil]))))

;; (<<with-substitutions [$$p (task-global "$$p")
;;                        $$mirror (task-global "$$mirror")]
;;   (local-select> (keypath *k) $$p :> *v)
;;   ...)
;;
;; becomes...
;; (let [$$p (task-global "$$p")
;;       $$mirror (task-global "$$mirror")]
;;   (let [*v (trampoline local-select> (keypath *k) $$p)]
;;     ...))
(defmethod handle-form '<<with-substitutions
           [node following ramavars]
           (let [metadata (meta node)
                 [_token bindings & body] (:children node)
                 binding-pairs (partition 2 (:children bindings))
                 bound-vars (into #{} (map (comp :value first) binding-pairs))
                 ramavars (set/union ramavars bound-vars)
                 new-node (let-node
                           (:children bindings)
                           (transform-body body ramavars))]
                [(with-meta new-node metadata) following]))

(defmethod handle-form 'anchor>
           [node following ramavars]
           (let [anchor-node (second (:children node))
                 new-node    (let-node
                              [anchor-node (api/token-node nil)]
                              (transform-body following ramavars))]
                [(with-meta new-node (meta node)) nil]))

(defmethod handle-form '<<do
           [node following ramavars]
           (let [children     (rest (:children node))
                 body         (transform-body (concat children following) ramavars)
                 out-ramavars (apply set/union (map (comp ::ramavars meta) body))
                 ramavars     (set/union ramavars out-ramavars)
                 new-node     (api/list-node
                               (list*
                                (api/token-node 'do)
                                body))]
                [(with-meta new-node
                            (assoc (meta node)
                                   ::ramavars ramavars)) nil]))

(defmethod handle-form '+group-by
           [node following ramavars]
           (let [children (:children node)
                 new-node (api/list-node
                           (list* (transform-body
                                   (concat children following)
                                   ramavars)))]
                [(with-meta new-node (meta node)) nil]))

(defmethod handle-form 'clj!
           [node following _ramavars]
           [node following])

(defn transform-form*
      "Given a form, and all forms following it, transforms it such that Rama
  dataflow code's emits and other special forms will be rewritten as `let`s,
  with the following forms nested inside the body of the `let`.

  Ex.
  (identity 1 :> *x)
  (identity 2 :> *y)
  (+ *x *y :> *z)
  (println *z)
  ...

  becomes

  (let [*x (identity 1)]
    (let [*y (identity 2)]
      (let [*z (+ *x *y)]
        (println *z)
        ...)))
  "
      ([f following] (transform-form* f following #{}))
      ([f following ramavars]
       (when (api/list-node? f)
             (let [out (validate-form (first (:children f)))]
                  out))
       (if-let [children (and (not (api/token-node? f))
                              (not (api/keyword-node? f))
                              (:children f))]
               (let [{:keys [prefix branches]} (split-form f children ramavars)]
                    (if (seq branches)
                        (let [bodies   (mapv #(transform-body % ramavars) branches)
                              ramavars (when (and (> (count branches) 1)
                                                  (not (rama-contains?
                                                        '#{<<sources}
                                                        (:value (first children)))))
                          ;; Here we want to find any ramavars defined on every
                          ;; branch. The intersection of all the ramavars. Then
                          ;; we want to find each of those ramavars that's new,
                          ;; so that we can perform unification with them.
                                             (set/difference
                                              (reduce
                                               set/intersection
                                               (eliminate-unchecked-branches branches bodies))
                                              ramavars))]

                             (if (seq ramavars) ;; unified ramavars
                                 (let [follows (transform-body following ramavars)]
                                      [(with-meta
                                        (let-node
                                         (concat
                      ;; Unified bindings at the top of the let to handle
                      ;; (default> :unify false) branches that have unified
                      ;; vars injected, preventing unresolved symbol errors
                                          (mapcat
                                           (fn [ramavar]
                                               [(api/token-node ramavar)
                                                (api/token-node 'nil)])
                                           ramavars)
                      ;; The conditional, inserted as a binding to _ so
                      ;; unified vars get used (avoiding unused var warnings)
                                          [(api/token-node '_)
                                           (api/list-node
                                            (into prefix
                                                  (map #(inject-ramavars-map ramavars %)
                                                       bodies)))])
                                         follows)
                                        {::ramavars ramavars})
                                       nil])
                                 [(with-meta
                                   (api/list-node
                                    (apply concat prefix bodies))
                                   {::ramavars ramavars})
                                  following]))

                        (let [[expr out new-bindings rebinds new-vars found-anchors]
                              (if (api/list-node? f)
                                  (extract-emits children ramavars)
                                  [f])]
                             (if
                              (or new-bindings rebinds (not-empty found-anchors))
                              [(let [ramavars     (into ramavars new-vars)
                                     follows      (transform-body following ramavars)
                                     ramavars     (into ramavars (::ramavars (meta follows)))
                    ;; HACK: This insertion of a call to `trampoline` is
                    ;; actually important. For whatever reason, clojure-lsp
                    ;; doesn't provide the regular editor support for things
                    ;; getting docs on hover, or jump to definition and stuff
                    ;; for function calls. Arguments to the functions behave
                    ;; fine, but the function name itself was missing this. As
                    ;; such, this call to `trampoline` is inserted to ensure
                    ;; that the editor is able to provide this functionality.
                                     expr         (cons (api/token-node 'trampoline) expr)
                                     new-bindings (if rebinds
                                                      (case (count new-bindings)
                                                            0 []
                                                            1 [(api/token-node (first new-bindings))
                                                               (api/list-node expr)]
                                                            [(api/vector-node
                                                              (mapv api/token-node new-bindings))
                                                             (api/list-node expr)])
                                                      [(if (= 1 (count out))
                                                           (first out)
                                                           (api/vector-node out))
                                                       (api/list-node expr)])
                                     rebindings   (mapcat
                                                   #(vector
                                                     (api/token-node %)
                                                     (api/list-node
                                                      [(api/token-node 'identity) %]))
                                                   rebinds)
                                     anchor-binds (mapcat #(vector
                                                            %
                                                            (api/token-node nil))
                                                          found-anchors)
                                     bind-expr    (with-meta
                                                   (let-node
                                                    (concat anchor-binds
                                                            new-bindings
                                                            rebindings)
                                                    follows)
                                                   {::ramavars ramavars})
                    ;; Produces
                    ;; (do
                    ;;   ...val
                    ;;   (let [...vars ...(str vars)]
                    ;;     ...follows))
                                     result
                                     (with-meta
                                      (if rebinds
                                          (api/list-node
                                           (list
                                            (api/token-node 'do)
                                            (api/list-node expr)
                                            bind-expr))
                                          bind-expr)
                                      {::ramavars ramavars})]
                                    result)
                               nil]
                              (let [handled-form (handle-form f following ramavars)]
                                   (if handled-form
                                       handled-form
                                       [(let [;; HACK: same as above, we insert a call to trampoline
                        ;; for other forms too. However, we need to make sure
                        ;; that we're NOT inserting it for `let` forms, because
                        ;; that would break bindings
                                              children (if (and (api/list-node? f)
                                                                (not (api/keyword-node? (first
                                                                                         children)))
                                                                (not (contains?
                                                                      '#{let letfn}
                                                                      (:value (first children)))))
                                                           (cons (api/token-node 'trampoline) children)
                                                           children)
                                              body     (transform-body children ramavars)]
                                             (vary-meta
                                              (cond
                                               (api/vector-node? f) (api/vector-node body)
                                               (api/map-node? f) (api/map-node body)
                                               (api/set-node? f) (api/set-node body)
                                               :else (api/list-node body))
                                              assoc
                                              ::ramavars
                                              (::ramavars (meta body))))
                                        following]))))))
               [f following])))

(defn transform-form
      ([f following] (transform-form* f following #{}))
      ([f following ramavars]
       (let [[_node _following :as out] (transform-form* f following ramavars)]
            out)))

(defn transform-body
      "Transform a sequence of rewrite nodes representing Rama forms.

  Takes a seq of rewrite nodes and returns a transformed sequences of nodes."
      ([body] (transform-body body #{}))
      ([body ramavars]
       (loop [body     body
              ramavars ramavars
              result   []]
             (if (seq body)
                 (let [[r following] (transform-form (first body) (rest body) ramavars)
                       r-ramavars (::ramavars (meta r))]
                      (recur following
                             (into ramavars r-ramavars)
                             (vary-meta (conj result r)
                                        update
                                        ::ramavars
                                        set/union
                                        (::ramavars (meta result))
                                        r-ramavars)))
                 result))))

(def module-declaration-forms
     '#{declare-depot declare-tick-depot declare-object declare-pstate mirror-depot
        mirror-pstate mirror-query})

(defn- wrap-with-topology-ref
       "Wraps a transformed node in (do (pr topology) node) so the topology
  argument appears used in the enclosing scope."
       [topology-node transformed-node]
       (with-meta
        (api/list-node
         (list (api/token-node 'do)
               (api/list-node
                (list (api/token-node 'pr) topology-node))
               transformed-node))
        (meta transformed-node)))

(defn transform-module-form
      "Given a form, and all forms following it, transforms it such that the above
  `declare-xyz` forms will be rewritten as `let`s, with the following forms
  nested inside the body of the `let`.

  Ex.
  (declare-depot s *depot (hash-by identity))
  (declare-depot s *depot-2 (hash-by key))
  ...

  becomes

  (let [*depot (declare-depot s (hash-by identity))]
    (let [*depot-2 (declare-depot s (hash-by key))]
      ...))
  "
      [form following pobjects]
      (if-let [children (and (not (api/token-node? form))
                             (not (api/keyword-node? form))
                             (:children form))]
              (cond
      ;; These forms all contain the same rewrite rules:
      ;;   - the first argument is the `setup` or topology binding
      ;;   - the second argument is the actual pobject (depot, pstate, etc)
      ;; We take the second argument, bind it in a `let` to the `declare-xyz`
      ;; form, then take everything proceeding it and put it in the body of the
      ;; `let` form.
      ;; NOTE: the weird extra call to `pr` that's inserted in the `let` is to
      ;; avoid unused bindings warnings that clj-kondo would usually provide if
      ;; the pobject wasn't directly referenced in that `module` scope.
      ;; Ex. if a pstate is only referenced inside a `defgenerator`
               (rama-contains? module-declaration-forms
                               (:value (first children)))
               (let [[declare on name & definition] children
                     pobject-name (:value name)
                     pobjects     (conj pobjects pobject-name)
                     follows      (transform-module-body following pobjects)
                     pobjects     (set/union pobjects (::pobjects (meta follows)))]
                    (err/maybe-missing-pobject-name name declare (meta form))
                    [(with-meta
                      (let-node
                       [name
                        (api/list-node (list* declare on definition))]
             ;; Extra pr call to avoid unused binding warnings when
             ;; the pobject is only referenced inside a defgenerator
                       (cons (api/list-node (list (api/token-node 'pr) name))
                             follows))
                      {::pobjects pobjects})
                     nil])

      ;; Anything inside `<<sources` or `<<query-topology` is data-flow code
      ;; for topologies, so that should be parse/re-written as Rama code
               (rama= '<<sources (:value (first children)))
               (let [[out _following] (binding [*context* :dataflow]
                                               (transform-form form [] pobjects))]
                    [out following])
               (rama= '<<query-topology (:value (first children)))
               (let [[out _following] (binding [*context* :dataflow]
                                               (handle-form form [] pobjects))]
                    [out following])

               :else
               [(let [body (transform-module-body children pobjects)]
                     (vary-meta
                      (cond
                       (api/vector-node? form) (api/vector-node body)
                       (api/map-node? form) (api/list-node body)
                       :else (api/list-node body))
                      assoc
                      ::pobjects
                      (::pobjects (meta body))))
                following])
              [form following]))

(defn transform-module-body
      "The rules for rewriting a module definition are similar to that of dataflow
  code in the sense that we're attempting to change linear-declarations into
  lexically-scoped `let` expressions. However, normal Rama code isn't allowed
  inside modules outside of `<<sources`, so we have to define this separately

  Note: For reference, the `transform-module-form` works very similarly to
  `transform-form`, however the implementation is a little simpler, so might be
  easier to get familiar with it first."
      [body pobjects]
      (loop [body     body
             pobjects pobjects
             result   []]
            (if (seq body)
                (let [[r following]
                      (transform-module-form (first body) (rest body) pobjects)
                      r-pobjects (::pobjects (meta r))]
                     (recur following
                            (into pobjects r-pobjects)
                            (vary-meta (conj result r)
                                       update
                                       ::pobjects
                                       set/union
                                       (::pobjects (meta result))
                                       r-pobjects)))
                result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn defbasicblocksegmacro-hook
      "Transform a Rama `defbasicblocksegmacro`

  Turns the form into a Clojure `defn`."
      [{:keys [node]}]
      (let [[_ macro-name & other] (:children node)
            [binding body] (if (api/string-node? (first other))
                               [(second other) (vec (drop 2 other))]
                               [(first other) (vec (rest other))])
            new-node       (with-meta
                            (api/list-node
                             (list*
                              (api/token-node 'defn)
                              macro-name
                              (api/vector-node
                               (filterv
                                (complement :k)
                                (:children binding)))
                              body))
                            (meta node))]
           {:node (with-meta new-node (meta node))}))

(defn defsegmacro-hook
      "Transform a Rama `defsegmacro`

  Turns the form into a Clojure `defn`"
      [{:keys [node]}]
      (let [m        (meta node)
            [_ name input & body] (:children node)
            input    (->> (:children input)
                          (filterv #(symbol? (:value %)))
                          (api/vector-node))
            new-node (api/list-node
                      (list* (api/token-node 'defn)
                             name
                             input
                             body))]
           (err/maybe-missing-def-name name m)
           (err/maybe-missing-input-vector input m)
           {:node (with-meta new-node m)}))

(defn deframaop-hook
      "Transform a Rama `deframaop` or `deframafn`

  Turns the form into a Clojure `defn`."
      [{:keys [node]}]
      (let [m        (meta node)
            [_ name & name-and-rest] (:children node)]
    ;; Validate name first
           (err/maybe-missing-def-name name m)

    ;; Detect and extract optional docstring between name and input vector
           (let [has-docstring? (and (seq name-and-rest) (api/string-node? (first name-and-rest)))
                 docstring (when has-docstring? (first name-and-rest))
                 input (if has-docstring? (second name-and-rest) (first name-and-rest))
                 children (if has-docstring? (drop 2 name-and-rest) (rest name-and-rest))]
      ;; Validate input vector after parsing
                (err/maybe-missing-input-vector input m)

      ;; Build defn form with optional docstring
                (let [defn-parts (concat [(api/token-node 'defn) name]
                                         (when has-docstring? [docstring])
                                         [input])
                      new-node (api/list-node
                                (concat defn-parts
                                        (binding [*context* :dataflow]
                                                 (transform-body children))))]
                     {:node (with-meta new-node m)}))))

(defn defoperation-hook
      "Transform a Rama `defoperation`

  Turns the form into a Clojure `defn`
  The output streams are bound in a `let` at the top of the function."
      [{:keys [node]}]
      (let [[_ op-name bindings args & body] (:children node)
            new-node (api/list-node
                      (list
                       (api/token-node 'defn)
                       op-name
                       args
                       (api/list-node
                        (list*
                         (api/token-node 'let)
                         bindings
                         body))))]
           {:node (with-meta new-node (meta node))}))

(defn top-level-block-hook
      "Transform a Rama `?<-`

  Turns the form into a Clojure `do`."
      [{:keys [node]}]
      (let [[_ & body] (:children node)
            new-node   (with-meta
                        (api/list-node
                         (list*
                          (api/token-node 'do)
                          (binding [*context* :dataflow]
                                   (transform-body body))))
                        (meta node))]
           {:node (with-meta new-node (meta node))}))

(defn batch<--hook
      "Transforms a Rama `batch<-`

  Turns the form into a Clojure `fn`"
      [{:keys [node]}]
      (let [metadata   (meta node)
            [new-node] (binding [*context* :dataflow]
                                (transform-body [node]))]
           {:node (with-meta new-node metadata)}))

(defn module-hook
      "Transforms an anonymous Rama `module`

  Turns the form into a Clojure `fn`"
      [{:keys [node]}]
      (let [children     (rest (:children node))
        ;; Determine if this module is passed an options map
            [input body] (if (api/map-node? (first children))
                             (let [[_options input & body] children]
                                  [input body])
                             (let [[input & body] children]
                                  [input body]))]
           (err/maybe-missing-input-vector input (meta node))
           {:node (with-meta (api/list-node
                              (list*
                               (api/token-node 'fn)
                               input
                               (transform-module-body body #{})))
                             (meta node))}))

(defn defmodule-hook
      "Transforms a named Rama `defmodule`

  Turns the form into a Clojure `defn`"
      [{:keys [node]}]
      (let [m        (meta node)
            children (rest (:children node))
            [name input body] (if (api/map-node? (second children))
                                  (let [[name _options input & body] children]
                                       [name input body])
                                  (let [[name input & body] children]
                                       [name input body]))
            new-node (with-meta
                      (api/list-node
                       (list*
                        (api/token-node 'defn)
                        name
                        input
                        (transform-module-body body #{})))
                      (meta node))]
           (err/maybe-missing-def-name name m)
           (err/maybe-missing-input-vector input m)
           {:node (with-meta new-node m)}))

(defn query-topology-hook
      "Transforms a Rama `<<query-topology` when used outside a defmodule body.

  Delegates to the existing handle-form multimethod which converts the form
  into a Clojure `fn`."
      [{:keys [node]}]
      (let [topology (second (:children node))
            [new-node] (binding [*context* :dataflow]
                                (handle-form node [] #{}))]
           {:node (wrap-with-topology-ref topology new-node)}))

(defn sources-hook
      "Transforms a Rama `<<sources` when used outside a defmodule body.

  Delegates to transform-form which dispatches to the split-form
  multimethod, splitting source blocks into separate branches."
      [{:keys [node]}]
      (let [topology (second (:children node))
            [new-node] (binding [*context* :dataflow]
                                (transform-form node [] #{}))]
           {:node (wrap-with-topology-ref topology new-node)}))

(defn foreign-select-hook
      "Validates that lambda functions aren't used in foreign-select calls"
      [{:keys [node] :as orig}]
      (binding [*context* :foreign-select]
               (->> (api/sexpr node)
                    (flatten)
                    (mapv validate-form)))
      orig)
