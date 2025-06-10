(ns janus.interpreter
  (:refer-clojure :exclude [name])
  (:require
   [janus.ast :as ast]
   [janus.env :as env]
   [janus.debug :as debug :refer [trace!]]))

(declare walk walk1)

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

;;;;; Application

(defn apply-μ [app]
  (let [μ (:head app)]
    (env/bind (:body μ) (:name μ) μ (:params μ) (:tail app))))

(defn apply-macro [app]
  ;; Pass the entire Application in so that we can delay.
  ((:f (:head app)) app))

(defn apply-primitive [app]
  (let [h    (:head app)
        tail (walk (:tail app))]
    ;; REVIEW: This assumes that all primitives take a list as args.
    ;; That seems innocuous, but what are the ramifications?
    (if (evaluated? tail)
      (let [args (env/list-expand tail)]
        (if ((:check h) args)
          ((:fn h) args)
          (ast/application h args)))
      (ast/application h tail))))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [app]
  (ast/application (walk (:head app)) (:tail app)))

;;;;; Eval

(defn eval-symbol [im]
  (let [s (:form im)]
    (if-let [ref (env/lookup s)]
      (do
        (trace! "Resolved" s ":" ref)
        ref)
      (do
        (trace! "Cannot resolve" s "postponing")
        im))))

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-pair [im]
  (let [p (:form im)]
    (ast/application (ast/immediate (:head p)) (:tail p))))

(defn eval-step [im]
  (update im :form walk))

;;;;; Reduction

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
   [:I :I] eval-step
   [:I :A] eval-step
   [:I :C] eval-step

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

   [:I :S] env/resolve ; lookup symbol in environment

   ;; REVIEW: It's nice to split the env logic out into its own module, but we
   ;; also need to generalise and split out the driving logic so as not to worry
   ;; about diversions.
   :C env/walk})

(def rule-tree
  (reduce (fn [acc [k v]]
            (assoc-in acc (if (vector? k) (conj k :fn) [k :fn]) v))
          {} rules))

(defn step [x]
  (cond
    (instance? janus.ast.Immediate x)   (:form x)
    (instance? janus.ast.Application x) (:head x)
    true                                nil))

(defn rule-match [sexp]
  (let [t1 (env/type sexp)]
    (if-let [subtree (get rule-tree t1)]
      (let [subexp (step sexp)
            t2 (env/type subexp)]
        (if-let [subsubtree (get subtree t2)]
          [[t1 t2] (:fn subsubtree)]
          [t1 (:fn subtree)]))
      [t1 identity])))

(defn trace-env [sexp]
  (let [syms (ast/symbols sexp)]
    (merge
     (into {} (map (fn [x] [x :unbound]) syms))
     #_(into {} (filter #(contains? syms (key %))) (env/names env)))))

(defn walk* [sexp]
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp "\n  env:" (trace-env sexp))
    (let [v (f sexp)]
      (trace! "result:" rule "\n" sexp "\n->\n" v)
      [rule v])))

(def walk1 (memoize walk*))

(defn walk
  ([env sexp]
   (walk (env/pin sexp env)))
  ([sexp]
   (let [[rule v] (walk1 sexp)]
     (if (= v sexp)
       sexp
       ;; We never change the env unless we hit a context switch.
       (recur (debug/tag v rule sexp))))))

;;;;; Builtins

(defn μ [app]
  (let [tail (:tail app)
        args (if (evaluated? tail) tail (walk1 tail))]
    (if (not (evaluated? args))
      (assoc app :tail args)
      (let [[name params body] (case (count args)
                                 3 args
                                 2 `[nil ~@args])
            [name params body] (if (ast/symbol params)
                                 [name params body]
                                 ;; REVIEW: Should these all be walk1?
                                 ;; We'd need repeat until fixedpoint logic.
                                 [(walk name) (walk params) (walk1 body)])
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
  (boolean? (env/base (nth (env/base args) 0))))

(defn select [[p t f]]
  ;; `t` & `f` have already been walked, so we've nothing to do but pick one.
  (if p t f))
