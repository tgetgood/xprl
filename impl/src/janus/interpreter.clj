(ns janus.interpreter
  (:refer-clojure :exclude [resolve])
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug :refer [trace!]]
   [janus.env :as env]))

;; REVIEW: Dynamic env massively simplifies the interpreter, but it breaks
;; memoisation.
;;
;; I guess I could memoise on form and *env*... would that work?
(def ^:dynamic *env* env/empty-ns)

(declare walk)

;;;;; Application

(defn apply-μ [{{{body :form env :env} :body :as μ} :head :as app}]
  (env/pin body (env/merge-envs *env*
                                (env/bind env (merge {(:params μ) (:tail app)}
                                                     (when-let [name (:name μ)]
                                                       {name μ}))))))

(defn apply-external [{{f :fn} :head :as app}]
  ;; REVIEW: We really do nothing with externals except send them messages and
  ;; connect channels.
  (f app))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [app]
  (update app :head walk))

;;;;; Eval

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-map [{m :form :as i}]
  (into (empty m) (map (fn [[k v]] [(assoc i :form k) (assoc i :form v)])) m))

(defn eval-seq [{{:keys [elements] :as seq} :form :as im}]
  (update seq :elements (partial mapv #(assoc im :form %))))

(defn eval-pair [im]
  (let [p (:form im)]
    (ast/application
     (ast/immediate (:head p))
                     (env/pin (:tail p) (assoc *env* :declarations #{})))))

(defn eval-inner
  "Walk inner form first, then come back to `x`."
  [x]
  (update x :form walk))

;;;;; Reduction

(defn walk-body [form] ; You ~could~ walk-all for μ & ν but why bother?
  (update form :body walk))

(defn walk-all [x]
  (reduce (fn [x k] (update x k walk)) x (keys x)))

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

(defn walk-list [l]
  (ast/list (map walk l)))

(defn walk-map [m]
  (into (empty m) (map (fn [[k v]] [(walk k) (walk v)])) m))

;;;;; Env

(defn resolve [{sym :form :as im}]
  (let [v (env/resolve *env* sym)]
    (if (= v ::env/unresolved)
      im
      v)))

(defn walk-in-context [{:keys [form env] :as ctx}]
  (debug/trace! "context switch:" env)
  (if (env/context-free? ctx)
    form ; don't bother evaluating fixed points.
    (binding [*env* (env/merge-envs *env* env)]
      (env/pin (walk form) *env*))))

(defn eval-in-context [{{form :form :as ctx} :form :as im}]
  (assoc ctx :form (assoc im :form form)))

;; REVIEW: Is this lazy or brilliant? Both?
(defn spread-context [{xs :form env :env}]
  (ast/list (map #(env/pin % env) xs)))

;;;;; Tree walker

(defn inconceivable? [& args]
  (throw (RuntimeException. "I thought this was unreachable!")))

(def rules
  {[:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :M] eval-map    ; (I {x y ...}) => {(I x) (I y) ...}
   [:I :I] eval-inner
   [:I :A] eval-inner

   [:I :seq]  eval-seq
   [:I :conc] eval-seq

   [:I :S] resolve

   [:I :V] :form     ; (I V) => V. values are fixed points of eval.

   :μ walk-body ; Walk has to recur into some structures, but most are data
   :ν walk-body
   :E walk-all
   :P walk-all

   :M    walk-map
   :L    walk-list
   :seq  walk-sequential
   :conc walk-all

   :C      walk-in-context
   [:I :C] eval-in-context ; => [:C :I]
   [:A :C] apply-head
   [:C :L] spread-context

   ;; TODO: An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; Somehow, the emission has to percolate up to the top level so that the
   ;; runtime can see it...
   ;;
   ;; I could just disallow this and require the programmer to jump through a
   ;; (ν ccs (apply (connect ... ccs) tail)) shaped hoop... but I don't like it.
   ;; [:A :E] apply-emit

   [:A :I] apply-head ; (A head tail) => (A (walk head) tail)
   [:A :A] apply-head ;   iff `head` is unevaluated.
   [:A :F] apply-external
   [:A :μ] apply-μ

   :A apply-error ; REVIEW: Should application be extensible? Dubious.
   })

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

(def default-types
  {clojure.lang.PersistentVector   :L
   clojure.lang.PersistentArrayMap :M
   clojure.lang.PersistentHashMap  :M
   clojure.lang.PersistentHashSet  :set})

(defn node-type [x]
  (let [t (type x)]
    (get (merge ast/type-table env/type-table default-types) t :V)))

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

(defn walk [sexp]
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp)
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
   (walk* (env/pin sexp env))))
