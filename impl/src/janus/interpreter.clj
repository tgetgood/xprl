(ns janus.interpreter
  (:refer-clojure :exclude [resolve])
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug :refer [trace!]]
   [janus.env :as env]))

(declare walk)

(defn evaluated? [x]
  (not (or (ast/immediate? x) (ast/application? x))))

(def ^:dynamic *μ-ctx* #{})

;;;;; Application

(defn apply-μ [{{:keys [body params name] :as μ} :head tail :tail :as app}]
  (if (ast/immediate? tail)
    ;; Eval immediates immediately, i.e., before sending.
    ;; We don't always have to, but there are cases where references can become
    ;; ambiguous if we don't.
    (update app :tail walk)
    (let [ext (merge {params tail} (when name {name μ}))]
      (debug/trace! "binding:" ext)
      (binding [*μ-ctx* (conj *μ-ctx* μ)]
        (walk (env/pin body ext))))))

(defn apply-external [{{f :fn} :head tail :tail :as app}]
  (if (evaluated? tail)
    (f app)
    (update app :tail walk)))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [{:keys [head tail] :as app}]
  (let [h (walk head)
        t (if (evaluated? head) tail (walk tail))]
    (assoc app :head h :tail t)))

;;;;; Eval

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-map [{m :form :as i}]
  (reduce (fn [m [k v]] (assoc m (assoc i :form k) (assoc i :form v))) (empty m) m))

(defn eval-seq [{{:keys [elements] :as seq} :form :as im}]
  (update seq :elements (partial mapv #(assoc im :form %))))

(defn eval-pair [{{:keys [tail head]} :form}]
  (ast/application (ast/immediate head) tail))

;;;;; Reduction

(defn walk-keys [& ks]
  (fn [x] (reduce (fn [x k] (update x k walk)) x ks)))

(defn walk-all [x]
  (reduce (fn [x k] (update x k walk)) x (keys x)))

(defn walk-sequential
  "Walks a seq in order, making sure each element has halted before walking the
  next."
  ;; FIXME: This stands out like a bad onion
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
  (reduce (fn [m [k v]] (assoc m (walk k) (walk v))) (empty m) m))

;;;;; Env

(defn resolve [{sym :form :as im}]
  (if (ast/resolved? sym)
    (if (contains? *μ-ctx* (:form sym))
      (throw (RuntimeException. "short circuit"))
      (:form sym))
    im))

;;;;; Tree walker

(def rules
  {[:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :M] eval-map    ; (I {x y ...}) => {(I x) (I y) ...}
   [:I :I] walk-all
   [:I :A] walk-all

   [:I :seq]  eval-seq
   [:I :conc] eval-seq

   [:I :S] resolve

   [:I :V] :form     ; (I V) => V. values are fixed points of eval.

   ;; Walk has to recur into some structures. How bad would it be if we just
   ;; made it walk into everything that isn't a value this way? How do we know
   ;; what's a value?
   :μ (walk-keys :body)
   ;; :ν (walk-keys :body)
   :E walk-all
   :P walk-all
   :R (walk-keys :form)

   :M    walk-map
   :L    walk-list
   :seq  walk-sequential
   :conc walk-all

   ;; TODO: An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; Somehow, the emission has to percolate up to the top level so that the
   ;; runtime can see it...
   ;;
   ;; I could just disallow this and require the programmer to jump through a
   ;; (ν ccs (apply (connect ... ccs) tail)) shaped hoop... but I don't like it.
   ;; [:A :E] apply-emit

   [:A :I] apply-head
   [:A :A] apply-head
   [:A :F] apply-external
   [:A :μ] apply-μ

   :A apply-error} )

(def rule-tree
  (reduce (fn [acc [k v]]
            (assoc-in acc (if (vector? k) (conj k :fn) [k :fn]) v))
          {} rules))

(defn step [x]
  (cond
    (ast/immediate? x)   (:form x)
    (ast/application? x) (:head x)
    true                 nil))

(defn unwind [rule trees]
  (cond
    (contains? (last trees) :fn) [rule (:fn (last trees))]
    (= 1 (count rule))           [rule identity]

    true (recur (into [] (butlast rule)) (into [] (butlast trees)))))

(defn rule-match
  ([s] (rule-match [] [rule-tree] s))
  ([rule trees sexp]
   (let [rule  (conj rule (ast/type sexp))
         trees (conj trees (get (last trees) (last rule)))]
     (if (last trees)
       (recur rule trees (step sexp))
       (unwind rule trees)))))

(defn walk1 [sexp]
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp)
    (let [v (f sexp)]
      (when (= v :error)
        (assert false ":error panic"))
      (trace! "result:" rule "\n" sexp "\n->\n" v)
      (debug/tag v rule sexp))))

(defn walk*
  ([sexp]
   (trace! "\n  pass:\n")
   (let [next (walk1 sexp)]
     (try
       (cond
         (= sexp next)                 sexp
         (nil? next)                   (assert false "inconceivable!")
         true                          (walk* next))
       (catch RuntimeException e
         (do
           (debug/trace! "\n ---Short Circuit!---\n" sexp)
           sexp)))))
  ([env sexp]
   (walk* (env/pin sexp env))))

;; (def walk (memoize walk1))
(def walk walk*)
