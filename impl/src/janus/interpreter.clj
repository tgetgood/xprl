(ns janus.interpreter
  (:refer-clojure :exclude [name])
  (:require
   [janus.ast :as ast]
   [janus.env :as env]
   [janus.debug :as debug :refer [trace!]]))

(declare walk)
(def ^:dynamic *env*)

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

;;;;; Application

(defn apply-μ [app]
  (let [μ (:head app)]
    (env/bind (:body μ) (:name μ) μ (:params μ) (env/pin (:tail app) *env*))))

(defn apply-primitive [app]
  (let [h    (:head app)
        args (walk (:tail app))]
    (trace! "pcall" h args)
    ;; REVIEW: This assumes that all primitives take a list as args.
    ;; That seems innocuous, but what are the ramifications?
    (if (and (ast/list? args) ((:check h) args))
      ((:fn h) args)
      (ast/application h args))))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [app]
  (ast/application (walk (:head app)) (:tail app)))

;;;;; Eval

(defn eval-symbol [im]
  (let [s (:form im)]
    (if-let [ref (env/lookup *env* s)]
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
  (ast/immediate (walk (:form im))))

;;;;; Reduction

(defn reduce-μ [μ]
  (let [n (when-let [n (:name μ)] (walk n))
        p (walk (:params μ))]
    (ast/μ n p (walk (env/declare (:body μ) n p)))))

(defn reduce-emit [e]
  ;; If we had a predicate that asked "is `kvs` fully realised?" then we
  ;; wouldn't need this at all. I'm just not sure how to write that predicate,
  ;; and this seems simple enough that I don't need to worry about it.
  (ast/emission (walk (:kvs e))))

(defn reduce-list [l]
  (ast/list (map walk l)))

;;;;; Tree walker

(def rules
  {[:I :S] eval-symbol ; aka lookup in environment
   [:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-step
   [:I :A] eval-step

   :I :form       ; (I V) => V. values are fixed points of eval.

   :L reduce-list ; Most structures are values, with these exceptions.
   :μ reduce-μ    ; TODO: Drop `reduce`. It's too common a term to override.
   :E reduce-emit ; REVIEW: Do we actually need to reduce into Emissions?

   [:A :I] apply-head ; (A head tail) => (A (walk head) tail)
   [:A :A] apply-head ;   iff `head` is unevaluated.

   ;; An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; [:A :E] apply-emit

   [:A :F] apply-primitive ; Two kinds of operators are built in.
   [:A :μ] apply-μ         ; I think that's sufficient. I might be wrong.

   :A apply-error}) ; REVIEW: Should application be extensible?

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
  (let [t1 (ast/type sexp)]
    (if-let [subtree (get rule-tree t1)]
      (let [subexp (step sexp)
            t2 (ast/type subexp)]
        (if-let [subsubtree (get subtree t2)]
          [[t1 t2] (:fn subsubtree)]
          [t1 (:fn subtree)]))
      [t1 identity])))

(defn trace-env [sexp]
  (merge
   (into {} (map (fn [x] [x :unbound]) (ast/symbols sexp)))
   (env/names *env*)))

(defn walk* [sexp]
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp "\n  env:" (trace-env sexp))
    (let [v (f sexp)]
      (trace! "result:" rule "\n" sexp "\n->\n" v )
      [rule v])))

(def walk** (memoize walk*))

(defn walk
  ([env sexp]
   (binding [*env* env]
     (walk sexp)))
  ([sexp]
   (if (env/switch? sexp)
     ;; Deal with context switches here rather than in rules.
     (let [result (binding [*env* (env/merge-env (:env sexp) *env*)]
                    (walk (debug/tag (:form sexp) :c sexp)))]
       (env/rewrap result sexp))
     (let [[rule v] (walk** sexp)]
       (if (= v sexp)
         ;; Caller never cares about the env as such.
         sexp
         ;; We never change the env unless we hit a context switch.
         (recur (debug/tag v rule sexp)))))))

;;;;; Builtins

(defn μ-ready? [args]
  (case (count args)
    2 (ast/symbol? (first args))
    3 (and (ast/symbol? (first args)) (ast/symbol? (second args)))))

(defn μ [args]
  (let [[name params body] (case (count args)
                             3 args
                             2 `[nil ~@args])]
    (ast/μ name params (env/declare body name params))))

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
