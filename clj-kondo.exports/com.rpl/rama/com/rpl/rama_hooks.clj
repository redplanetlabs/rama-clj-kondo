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
   (into #{} (comp (map pstate-or-nil) (remove nil?)))
   (into [])))

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

(defn wrap-do
  "Wraps an expression in a `do`"
  [body]
  (if (= 1 (count body))
    (first body)
    (api/list-node
     (list*
      (api/token-node 'do)
      body))))

(defn wrap-<<do
  "Exact same thing as above, except using in contexts where we expect the
  result to be passed to `transform-body`"
  [body]
  (if (= 1 (count body))
    (first body)
    (api/list-node
     (list*
      (api/token-node '<<do)
      body))))


(defn- inject-ramavars-map
  [ramavars body]
  (let [fval (:value (second (:children (first body))))]
    (if (rama-contains? '#{case> default>} fval)
      (first body)
      (wrap-do
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
       ;; Remove any `case>` and `default>` nodes
       (remove
        (fn [[[branch]]]
          (rama-contains? '#{case> default>}
                          (:value (first (:children branch))))))
       (mapv (comp ::ramavars meta second))
  ))

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
  "Separate the output vars from an expression."
  [terms curr-ramavars]
  (let [[exprs zero-arity-out out] (partition-by ; split output streams
                                    (fn [x] (= :> (:k x)))
                                    (rest terms))
        [exprs out] (if (and (api/keyword-node? (first exprs))
                             (= :> (:k (first exprs))))
                      [[] zero-arity-out]
                      [exprs out])
        ramavars    (find-all-ramavars out)
        new-vars    (set/difference ramavars curr-ramavars)
        rebind      (set/intersection ramavars curr-ramavars)]
    [(concat (list (first terms)) exprs)
     out
     (when (seq new-vars) (into [] new-vars))
     (when (seq rebind) (into [] rebind))
     ramavars]
  ))

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
              marker-node        (api/list-node
                                  (list (api/token-node 'case>) type-node))]
          (err/maybe-subsource-case-arity (:children marker) metadata)
          [[marker-node] [(wrap-<<do (concat [marker] body))]]))
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
         [(wrap-<<do body)]])
      blocks)
    }))

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
               [[marker] [(wrap-<<do body)]])
      blocks)
    }))

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
       [[(wrap-<<do if-block)] [(wrap-<<do else-block)]]
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
          value)))
  ))

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
        new-node     (api/list-node
                      (list*
                       (api/token-node 'let)
                       (api/vector-node
                        [(api/token-node '_)
                         (api/list-node
                          (list* (api/token-node 'pr) (map api/token-node old)))
                         new-bindings
                         (api/vector-node [])])
                       (transform-body
                        (concat [use-ramavars] [expr] following)
                        ramavars)))
        metadata     (meta node)
       ]
    [(with-meta new-node metadata) nil]
  ))

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
           (api/list-node
            (list*
             (api/token-node 'let)
             (api/vector-node [(api/vector-node
                                (map api/token-node outvars))
                               (api/vector-node [])])
             (transform-body (cons fake-node following) ramavars)))
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
          (api/list-node
           (list*
            (api/token-node 'let)
            (api/vector-node
             (mapcat
              #(vector (api/token-node %)
                       (api/string-node (str %)))
              pstates))
            (transform-body (concat body [input]) ramavars)))))
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
        (extract-emits (:children input-output) #{})
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
        new-node
        (api/list-node
         (list*
          (api/token-node 'let)
          (api/vector-node
           [(api/vector-node outputs)
            (api/list-node
             (list*
              ;; NOTE: it would be more semantic to turn this into a `loop`
              ;; form, but then we'd get "loop without recur" warnings.
              (api/token-node 'let)
              (api/vector-node inputs)
              (transform-body body ramavars)))])
          (transform-body following ramavars)
         ))]
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
            new-node (api/list-node
                      (list*
                       (api/token-node 'let)
                       (api/vector-node [(api/token-node depot-name)
                                         (api/token-node nil)])
                       (transform-body (concat [node] following)
                                       (conj ramavars depot-name))))]
        [(with-meta new-node metadata) nil]))))

(defmethod handle-form 'anchor>
  [node following ramavars]
  (let [anchor-node (second (:children node))
        new-node    (api/list-node
                     (list*
                      (api/token-node 'let)
                      (api/vector-node [anchor-node (api/token-node nil)])
                      (transform-body following ramavars)))]
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

(defn transform-form
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
  ([f following] (transform-form f following #{}))
  ([f following ramavars]
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
                  (api/list-node
                   (list*
                    (api/token-node 'let)
                    (api/vector-node
                     (concat
                      ;; Stick the unified bindings up at the top of the `let`.
                      ;; This doesn't really matter, but it's to handle the
                      ;; specific case where there's a
                      ;; `(default> :unify false)`
                      ;; And the body of the branch ends up having the unified
                      ;; vars injected into it. Having the unified vars
                      ;; declared first will ensure this doesn't cause an
                      ;; unresolved symbol error
                      (mapcat
                       (fn [ramavar]
                         [(api/token-node ramavar)
                          ;; this needs to use the map from above?
                          (api/token-node 'nil)])
                       ramavars)
                      ;; Then we'll insert the actual conditional as a part of
                      ;; the let. Since the unified vars come before, this
                      ;; could also just be in the body of the `let`, but it
                      ;; doesn't really matter.
                      [(api/token-node '_)
                       (api/list-node
                        (into prefix
                              ;; We want to inject these expressions with just
                              ;; something that uses the unified vars to avoid
                              ;; getting unused var warnings
                              ;; NOTE: The best we can really do here is to
                              ;; avoid giving false warnings - however, we also
                              ;; can't really give real ones unless we wanted
                              ;; to duplicate the tree of proceeding segments
                              ;; and nest them under each branch, but that's
                              ;; not really a viable solution
                              (map #(inject-ramavars-map ramavars %)
                                   bodies)))]))
                    follows))
                  {::ramavars ramavars})
                nil])
             [(with-meta
                (api/list-node
                 (apply concat prefix bodies))
                {::ramavars ramavars})
              following]))

         (let [[expr out new-bindings rebinds new-vars]
               (if (api/list-node? f)
                 (extract-emits children ramavars)
                 [f])]
           (if
             (or new-bindings rebinds)
             [(let [ramavars     (into ramavars new-vars)
                    follows      (transform-body following ramavars)
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
                    bind-expr    (with-meta
                                   (api/list-node
                                    (list*
                                     (api/token-node 'let)
                                     (api/vector-node
                                      (if rebinds
                                        (concat new-bindings
                                                rebindings)
                                        new-bindings))
                                     follows))
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
                       :else (api/list-node body))
                     assoc
                     ::ramavars
                     (::ramavars (meta body))))
                  following]))))))
     [f following])))

(defn transform-body
  "Transform a sequence of rewrite nodes representing Rama forms.

  Takes a seq of rewrite nodes and returns a transformed sequences of nodes."
  ([body] (transform-body body #{}))
  ([body ramavars]
   (loop [body     body
          ramavars ramavars
          result   []]
     (if (seq body)
       (let [[r following] (transform-form (first body) (rest body) ramavars)]
         (recur following
                (into ramavars (::ramavars (meta r)))
                (vary-meta (conj result r)
                           update
                           ::ramavars
                           set/union
                           (::ramavars (meta result))
                           (::ramavars (meta r)))
         ))
       result))))

(def module-declaration-forms
  '#{declare-depot declare-tick-depot declare-object declare-pstate mirror-depot
     mirror-pstate mirror-query})

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
           (api/list-node
            (list*
             (api/token-node 'let)
             (api/vector-node
              [name
               (api/list-node (list* declare on definition))])
             ;; We want to add this extra "call" to pr here to avoid
             ;; getting warnings that the pstate is unused when it's
             ;; referenced in a defgenerator or something like that
             (api/list-node (list (api/token-node 'pr) name))
             follows))
           {::pobjects pobjects})
         nil])

      ;; Anything inside `<<sources` or `<<query-topology` is data-flow code
      ;; for topologies, so that should be parse/re-written as Rama code
      (rama= '<<sources (:value (first children)))
      (let [[out _following] (transform-form form [] pobjects)]
        [out following])
      (rama= '<<query-topology (:value (first children)))
      (let [[out _following] (handle-form form [] pobjects)]
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
    [form following]
  ))

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
            (transform-module-form (first body) (rest body) pobjects)]
        (recur following
               (into pobjects (::pobjects (meta r)))
               (vary-meta (conj result r)
                          update
                          ::pobjects
                          set/union
                          (::pobjects (meta result))
                          (::pobjects (meta r)))))
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
        [_ name input & children] (:children node)
        new-node (api/list-node
                  (list* (api/token-node 'defn)
                         name
                         input
                         (transform-body children)))]
    (err/maybe-missing-def-name name m)
    (err/maybe-missing-input-vector input m)

    {:node (with-meta new-node m)}))

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
                       (transform-body body)))
                     (meta node))]
    {:node (with-meta new-node (meta node))}))

(defn batch<--hook
  "Transforms a Rama `batch<-`

  Turns the form into a Clojure `fn`"
  [{:keys [node]}]
  (let [metadata   (meta node)
        [new-node] (transform-body [node])]
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
