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
    (env/bind (:body μ) (:name μ) μ (:params μ) (:tail app))))

(defn apply-macro [app]
  ;; Pass the entire Application in so that we can delay.
  ((:f (:head app)) app))

(defn apply-primitive [app]
  (let [h    (:head app)
        args (walk (:tail app))]
    ;; REVIEW: This assumes that all primitives take a list as args.
    ;; That seems innocuous, but what are the ramifications?
    (if (and (evaluated? args) ((:check h) args))
      ((:fn h) args)
      (ast/application h args))))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [app]
  (ast/application (walk (:head app)) (:tail app)))

;;;;; Eval

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-pair [im]
  (let [p (:form im)]
    (ast/application (ast/immediate (:head p)) (:tail p))))

;;;;; Reduction

(defn eval-inner
  "Walk inner form first, then come back to `x`."
  [x]
  (update x :form walk))

(defn reduce-μ [μ]
  (update μ :body walk))

(defn reduce-emit [e]
  ;; If we had a predicate that asked "is `kvs` fully realised?" then we
  ;; wouldn't need this at all. I'm just not sure how to write that predicate,
  ;; and this seems simple enough that I don't need to worry about it.
  (update e :kvs walk))

(defn reduce-list [l]
  (ast/list (map walk l)))

;;;;; Tree walker

(def rules
  {[:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-inner
   [:I :A] eval-inner
   [:I :C] eval-inner

   :I :form       ; (I V) => V. values are fixed points of eval.

   :L reduce-list ; Most structures are values, with these exceptions.
   :μ reduce-μ    ; TODO: Drop `reduce`. It's too common a term to override.
   :E reduce-emit ; REVIEW: Do we actually need to reduce into Emissions?

   [:A :I] apply-head ; (A head tail) => (A (walk head) tail)
   [:A :A] apply-head ;   iff `head` is unevaluated.
   [:A :C] apply-head

   ;; An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; [:A :E] apply-emit

   [:A :M] apply-macro
   [:A :F] apply-primitive ; Two kinds of operators are built in.
   [:A :μ] apply-μ         ; I think that's sufficient. I might be wrong.

   :A apply-error ; REVIEW: Should application be extensible?

   [:I :S] identity              ; unresolved symbols can't be evaluated
   [:I :R] (comp :binding :form) ; resolved symbols store their referrent

   [:C :S] env/resolve
   [:C :R] env/reresolve

   [:C :C] env/merge-ctx

   ;; REVIEW: It's nice to split the env logic out into its own module, but we
   ;; also need to generalise and split out the driving logic so as not to worry
   ;; about diversions.
   :C env/push-down})

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

(defn rule-match [sexp]
  (let [t1 (node-type sexp)]
    (if-let [subtree (get rule-tree t1)]
      (let [subexp (step sexp)
            t2 (node-type subexp)]
        (if-let [subsubtree (get subtree t2)]
          [[t1 t2] (:fn subsubtree)]
          [t1 (:fn subtree)]))
      [t1 identity])))

(defn trace-env [sexp]
  (ast/symbols sexp))

(defn walk1 [sexp]
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp "\n  syms:" (trace-env sexp))
    (let [v (f sexp)]
      (trace! "result:" rule "\n" sexp "\n->\n" v)
      [rule v])))

;; (def walk1 (memoize walk1))

(defn walk
  ([env sexp]
   (walk (env/pin sexp env)))
  ([sexp]
   (let [[rule v] (walk1 sexp)]
     (if (= v sexp)
       sexp
       (recur (debug/tag v rule sexp))))))

;;;;; Builtins

(defn μ [app]
  (let [tail (:tail app)
        args (if (evaluated? tail) tail (second (walk1 tail)))]
    (if (not (evaluated? args))
      (assoc app :tail args)
      (let [[name params body] (case (count args)
                                 3 args
                                 2 `[nil ~@args])
            [name params body] (if (ast/symbol? (env/peel params))
                                 [name params body]
                                 ;; REVIEW: Should these all be walk1?
                                 ;; We'd need repeat until fixedpoint logic.
                                 [(when name (walk name))
                                  (walk params)
                                  (second (walk1 body))])
            psym (env/peel params)]
        (if (ast/symbol? psym)
          (ast/μ name psym (env/declare body name psym))
          (assoc app :tail [name params body]))))))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (ast/emission
   (ast/list (map (fn [[k v]] (ast/list [(ast/immediate k) v]))
                  (partition 2 kvs)))))

(defn check-select [args]
  (boolean? (nth args 0)))

(defn select [[p t f]]
  ;; `t` & `f` have already been walked, so we've nothing to do but pick one.
  (if p t f))
