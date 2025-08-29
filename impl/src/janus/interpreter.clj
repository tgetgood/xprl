(ns janus.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug]
   [janus.env :as env]))

(declare walk)

(defn continue [x y]
  (cond
    (= x y)            x
    (ast/evaluated? y) y
    true               (walk y)))

(def ^:dynamic *μ-ctx* #{})
(def ^:dynamic *emit?* true)
(def ^:dynamic *cycle-break* ::uninitialised)

(defmacro prevent-emission [& body]
  `(binding [*emit?* false]
     ~@body))

;;;;; Application

(defn apply-μ [{{:keys [body params name] :as μ} :head tail :tail :as app}]
  (binding [*μ-ctx* (conj *μ-ctx* μ)]
    (walk (env/bind μ tail))))

(defn apply-external [{{f :fn n :name} :head tail :tail :as app}]
  (if (ast/evaluated? tail)
    (f app)
    (continue app (update app :tail walk))))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [{:keys [head tail] :as app}]
  (let [h (walk head)
        t (if (ast/evaluated? h) tail (walk tail))]
    (continue app (assoc app :head h :tail t))))

(def apply-rules
  {:I apply-head
   :A apply-head
   :F apply-external
   :μ apply-μ})

(defn apply [sexp]
  ((get apply-rules (ast/type (:head sexp)) apply-error) sexp))

;;;;; Eval

(defn walk-coll [xs]
  (continue xs (into (ast/empty xs) (map walk) xs)))

(defn eval-coll [xs]
  (continue xs (walk-coll xs)))

(defn eval-list [im]
  (walk (ast/list (map ast/immediate (:form im)))))

(defn eval-map [{m :form :as i}]
  (walk (reduce (fn [m [k v]] (assoc m (assoc i :form k) (assoc i :form v))) (empty m) m)))

(defn eval-pair [{{:keys [tail head]} :form}]
  (walk (ast/application (ast/immediate head) tail)))

(defn resolve [{{v :val :as sym} :form :as im}]
  (if (and (ast/resolved? sym) (not (nil? v)) (not (contains? *μ-ctx* v)))
    v
    im))

(def eval-rules
  {:P eval-pair     ; (I (P x y)) => (A (I x) y)
   :L eval-list     ; (I (L x y ...)) => (L (I x) (I y) ...)
   :M eval-map      ; (I {x y ...}) => {(I x) (I y) ...}
   :I walk-coll
   :A walk-coll
   :V :form         ; (I V) => V. values are fixed points of eval.
   :S resolve})

(defn eval [sexp]
  ((get eval-rules (ast/type (:form sexp))) sexp))

;;;;; Reduction

(defn walk-μ [μ]
  (continue μ (prevent-emission (update μ :body walk))))

(def walk-rules
  {:I eval
   :A apply
   :P walk-coll
   :L walk-coll
   :M walk-coll
   :μ walk-μ})

(defn walk* [sexp]
  (let [t     (ast/type sexp)
        next  ((get walk-rules t identity) sexp)]
    (debug/trace! "walk:" t "\n" sexp "\n->\n" next)
    next))

;;;;; REVIEW: Debugging is still a pain with memoisation.

;; (def walk (memoize walk*))
(def walk walk*)

(defn interpret [ns form]
  (walk (env/ns-set! ns form)))
