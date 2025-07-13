(ns janus.interpreter
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug :refer [trace!]]
   [janus.env :as env]))

(declare walk)

(defn evaluated? [x]
  (cond
    (ast/immediate? x)   false
    (ast/application? x) false
    (env/ctx? x)         false
    true                 true))

;;;;; Application

(defn apply-μ [app]
  (let [μ (:head app)]
    (env/bind (:body μ) (:id μ) (merge {(:params μ) (:tail app)}
                                       (when-let [name (:name μ)]
                                         {name μ})))))

(defn apply-primitive [app]
  (let [h    (:head app)
        args (walk (:tail app))]
    (if (and (evaluated? args) ((:check h) args))
      ((:fn h) args)
      (ast/application h args))))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [app]
  (update app :head walk))

;;;;; Eval

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-seq [{{:keys [elements] :as seq} :form :as im}]
  (update seq :elements (partial mapv #(assoc im :form %))))

(defn eval-pair [im]
  (let [p (:form im)]
    (ast/application (ast/immediate (:head p)) (:tail p))))

(defn eval-inner
  "Walk inner form first, then come back to `x`."
  [x]
  (update x :form walk))

;;;;; Reduction

(defn walk-keys [els]
  (fn [x]
    (reduce (fn [x k] (update x k walk)) x els)))

;; This isn't necessary, just an optimisation.
(def walk-body (walk-keys [:body]))

(defn walk-all [x]
  ((walk-keys (keys x)) x))

(defn walk-sequential
  "Walks a seq in order, making sure each element has halted before walking the
  next."
  [{xs :elements}]
  (loop [[x & xs] xs]
    (let [v (walk x)]
      (if (= v :end-of-computation)
        (if (seq xs)
          (recur xs)
          :end-of-computation)
        (ast/seq (into [v] xs))))))

;; This is an ugly necessity since we're using native vectors instead of our own
;; record type.
(defn walk-list [l]
  (ast/list (map walk l)))

;;;;; Env

(defn resolve-inner-binding [{{{inner :form :as od} :form :as ob} :form :as i}]
  (assoc ob :form (assoc od :form (walk (assoc i :form inner)))))

;;;;; Tree walker

(defn inconceivable? [& args]
  (throw (RuntimeException. "I thought this was unreachable!")))

(def rules
  {[:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-inner
   [:I :A] eval-inner

   [:I :seq]  eval-seq
   [:I :conc] eval-seq

   :I :form     ; (I V) => V. values are fixed points of eval.

   :μ walk-body ; Walk has to recur into some structures, but most are data
   :ν walk-body
   :E walk-all
   :P walk-all

   :L    walk-list
   :seq  walk-sequential
   :conc walk-all

   [:A :I] apply-head ; (A head tail) => (A (walk head) tail)
   [:A :A] apply-head ;   iff `head` is unevaluated.

   ;; An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; Somehow, the emission has to percolate up to the top level so that the
   ;; runtime can see it...
   ;;
   ;; I could just disallow this and require the programmer to jump through a
   ;; (ν ccs (apply (connect ... ccs) tail)) shaped hoop... but I don't like it.
   ;; [:A :E] apply-emit

   [:A :F] apply-primitive ; Two kinds of operators are built in.
   [:A :μ] apply-μ         ; I think that's sufficient. I might be wrong.

   :A apply-error ; REVIEW: Should application be extensible?

   [:I :S] identity              ; unresolved symbols can't be evaluated

   ;;;;; Environmental manipulation
   ;;
   ;; The fact that most of the rules are here says to me that this is overly
   ;; complicated, but I don't yet know how to simplify.

   [:I :C] eval-inner
   [:I :B] eval-inner
   [:I :D] eval-inner

   [:A :C] apply-head
   [:A :B] apply-head
   [:A :D] apply-head

   [:C :C] inconceivable?

   [:D :B] eval-inner
   [:B :D] eval-inner
   [:B :C] eval-inner
   [:C :D] eval-inner

   [:C :S] (fn [{sym :form ctx :ctx :as c}]
             (if (contains? (env/names ctx) sym)
               c
               sym))

   [:D :C] (fn [{{form :form :as c} :form :as d}]   ; -> [:C :D]
             (assoc c :form (assoc d :form form)))  ; i.e. invert the nodes.


   [:D :D] env/merge-decls
   [:B :B] env/merge-binds

   [:D :S] (fn [{sym :form syms :syms :as decl}]
             (if (contains? syms sym)
               (update decl :syms select-keys [sym])
               sym))

   [:C :D :S] env/c-or-d

   [:B :C :S] (fn [{{sym :form ctx :ctx :as c} :form bindings :bindings :as b}]
                (if (contains? bindings sym)
                  (assoc b :form sym)
                  c))

   [:B :D :S] env/simplify-bindings
   [:B :S]    :form ; Binding without declaration is a noop

   [:I :C :S] env/resolve

   [:I :D :B :D :S] (fn [{{b :form :as d} :form :as im}]
                      (assoc d :form (walk (assoc im :form b))))

   [:I :B :D :B :D :S] resolve-inner-binding
   [:I :B :D :S]       env/bind-arg

   ;; FIXME:
   ;; [:I :B :D :B :D :S] (fn [_] (throw (RuntimeException. "not implemented")))

   ;; FIXME: I need a regex style [:I (:B :D)+ :S] style rule. Probably [:I :D
   ;; (:B :D)+ :S] as well.
   ;;
   ;; That's getting nice and fugly.
   ;;
   ;; That probably means I need a new design. This one has gotten me
   ;; impressively far, but has some serious kinks. Can I ignore those and move
   ;; on to new problems for a bit (like ν)? I'm starting to lose steam over
   ;; this for now.

   :C env/push-down
   :D env/push-down
   :B env/push-down})

(def rule-tree
  (reduce (fn [acc [k v]]
            (assoc-in acc (if (vector? k) (conj k :fn) [k :fn]) v))
          {} rules))

(defn step [x]
  (cond
    (ast/immediate? x)   (:form x)
    (ast/application? x) (:head x)
    (env/ctx? x)         (:form x)
    true                 nil))

(defn node-type [x]
  (let [t (type x)]
    (get env/type-table t (get ast/type-table t :V))))

(defn unwind [rule trees]
  (cond
    (contains? (last trees) :fn) [rule (:fn (last trees))]
    (= 1 (count rule))           [rule identity]

    true (recur (into [] (butlast rule)) (into [] (butlast trees)))))

(defn rule-match
  ([s] (rule-match [] [rule-tree] s))
  ([rule trees sexp]
   (let [rule  (conj rule (node-type sexp))
         trees (conj trees (get (last trees) (last rule)))]
     (if (last trees)
       (recur rule trees (step sexp))
       (unwind rule trees)))))

(defn trace-env [sexp]
  (ast/symbols sexp))

(defn walk [sexp]
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp "\n  syms:" (trace-env sexp))
    (let [v (f sexp)]
      (trace! "result:" rule "\n" sexp "\n->\n" v)
      (debug/tag v rule sexp))))

;; (def walk (memoize walk1))

(defn walk*
  ([sexp]
   (trace! "\n  pass:\n")
   (let [next (walk sexp)]
     (cond
       (= sexp next) sexp
       (nil? next)   (assert false "inconceivable!")
       true          (recur next))))
  ([env sexp]
   (walk* (env/pin sexp (env/project env sexp)))))

;;;;; Builtins

(defn μ-ready? [args]
  (and
   (ast/list? args)
   (every? #(ast/symbol? (env/peel %)) (butlast args))))

(defn μ [args]
  (let [id    (gensym)
        names (into [] (map env/peel) (butlast args))]
    (apply ast/μ id (conj names (env/declare (last args) id names)))))

(defn ν [args]
  (let [params (env/peel (first args))
        body   (env/declare (last args) :ν [params])]
    ;; REVIEW: νs evaluate their bodies. I think that's the right thing.
    (ast/ν params (ast/immediate body))))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (ast/emission
   (ast/list (map (fn [[k v]] (ast/list [(ast/immediate k) v]))
                  (partition 2 kvs)))))

(defn check-select [args]
  (let [p (nth args 0)]
    (when (evaluated? p)
      (assert (boolean? p) (str "Non boolean passed to select: " p))
      true)))

(defn select [[p t f]]
  ;; `t` & `f` have already been walked, so we've nothing to do but pick one.
  (if p t f))
