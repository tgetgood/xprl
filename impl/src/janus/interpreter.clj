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

(defn apply-μ [{{:keys [body params name] :as μ} :head tail :tail :as app}]
  (let [args (env/pin tail *env*)]
    (binding [*env* (merge *env* (merge {params args} (when name {name μ})))]
      (walk body))))

(defn apply-external [{{f :fn} :head :as app}]
  ;; TODO: We should do a little more work here. External interpreters can't do
  ;; anything with interal references, so the tail should be reduced before
  ;; sending.
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
     (ast/immediate (:head p)) (:tail p))))

(defn eval-inner
  "Walk inner form first, then come back to `x`."
  [x]
  (update x :form walk))

;;;;; Reduction

(defn walk-body [{:keys [name params] :as form}]
  (binding [*env* (transduce (remove nil?) dissoc *env* [name params])]
    (update form :body walk)))

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
  (if (env/bound? *env* sym)
    (env/lookup *env* sym)
    im))

(defn walk-in-context [{:keys [form env] :as ctx}]
  (debug/trace! "context switch:" env)
  (binding [*env* env] (walk form)))

(defn eval-in-context [{{form :form :as ctx} :form :as im}]
  (assoc ctx :form (assoc im :form form)))

(defn apply-in-context [{head :head :as app}]
  (let [{h :form env :env} (walk head)
        res (walk (assoc app :head h))]
    (if (ast/application? res)
      ;; We need to restore the inner context node if application was postponed.
      (update res :head #(env/pin % env))
      res)))

(defn spread-context [{xs :form env :env}]
  (ast/list (map #(env/pin % env) xs)))

;;;;; Tree walker

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
   [:I :C] eval-inner ;-in-context  ; (I (C x)) =? (C (I x))
   [:A :C] apply-head ;-in-context ; (A (C h) t) => (C (A h (C t))) ; tail takes *env*
   ;; [:C :L] spread-context   ; REVIEW: This seems sloppy

   ;; [:I :C :C] #(update % :form :form) ; drop outer.

   ;; [:C :S] :form
   ;; [:C :V] :form
   ;; [:C :C] :form ; the inner context always wins
   ;; [:C :C] (fn [x] (throw (RuntimeException. "nested contexts are an error.")))

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

(defn walk1 [sexp]
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp)
    (let [v (f sexp)]
      (trace! "result:" rule "\n" sexp "\n->\n" v)
      (debug/tag v rule sexp))))


(defn walk*
  ([sexp]
   (trace! "\n  pass:\n")
   (let [next (walk1 sexp)]
     (cond
       (= sexp next) sexp
       (nil? next)   (assert false "inconceivable!")
       true          (recur next))))
  ([env sexp]
   (walk* (env/pin sexp env))))

;; (def walk (memoize walk1))
(def walk walk*)
