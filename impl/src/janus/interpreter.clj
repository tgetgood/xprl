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
    (env/bind (:body μ) (merge {(:params μ) (:tail app)}
                               (when-let [name (:name μ)]
                                 {name μ})))))

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
  (update app :head walk))

;;;;; Eval

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-pair [im]
  (let [p (:form im)]
    (ast/application (ast/immediate (:head p)) (:tail p))))

(defn eval-inner
  "Walk inner form first, then come back to `x`."
  [x]
  (update x :form walk))

;;;;; Reduction

(defn walk-μ [μ]
  (update μ :body walk))

(defn walk-emit [e]
  (update e :kvs walk))

(defn walk-list [l]
  (ast/list (map walk l)))

;;;;; Tree walker

(def rules
  {[:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-inner
   [:I :A] eval-inner
   [:I :C] eval-inner

   :I :form     ; (I V) => V. values are fixed points of eval.

   :L walk-list ; Walk has to recur into some structures, but most are data
   :μ walk-μ
   :E walk-emit

   [:A :I] apply-head ; (A head tail) => (A (walk head) tail)
   [:A :A] apply-head ;   iff `head` is unevaluated.
   [:A :C] apply-head

   ;; An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; [:A :E] apply-emit

   [:A :F] apply-primitive ; Two kinds of operators are built in.
   [:A :μ] apply-μ         ; I think that's sufficient. I might be wrong.

   :A apply-error ; REVIEW: Should application be extensible?

   [:I :S] identity              ; unresolved symbols can't be evaluated
   [:I :R] (comp :binding :form) ; resolved symbols store their referrent

   [:C :S] env/resolve
   [:C :R] env/reresolve

   [:D :B :I :I] ::not-implemented

   [:C :C] env/merge-ctx

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

(defn unwind [rule trees]
  (cond
    (contains? (last trees) :fn) [rule (:fn (last trees))]
    (= 1 (count rule))           [(first rule) identity]

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

;; (def walk (memoize walk))

(defn walk*
  ([env sexp]
   (loop [sexp (env/pin sexp env)]
     (let [next (walk sexp)]
       (if (= sexp next)
         sexp
         (recur next))))))

;;;;; Builtins

(defn μ-ready? [args]
  (and
   (ast/list? args)
   (ast/symbol? (first args))
   (or (= 2 (count args)) (ast/symbol? (second args)))))

(defn μ [args]
  (apply ast/μ (update args (dec (count args)) env/declare (butlast args))))

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
