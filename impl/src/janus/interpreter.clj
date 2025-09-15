(ns janus.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug]
   [janus.env :as env]))

(declare walk)

(defn simple
  "Wraps a function that just acts on a form to act on the form embedded in a
  state map."
  [f]
  (fn [state]
    (update state :form f)))

;;;;; Application

(defn apply-μ [{{μ :head tail :tail :as app} :form :as state}]
  ;; Interpreter as middleware. Kind of a cool idea?
  (-> state
      (update :μ-ctx (fnil conj #{}) μ)
      (assoc :form (env/bind μ tail))
      walk
      ;; Only undo things we did.
      (assoc :μ-ctx (:μ-ctx state))))


(defn apply-external [{{f :fn n :name} :head tail :tail :as app}]
  (if (ast/evaluated? tail)
    (f app)
    (update app :tail walk)))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [{:keys [head tail] :as app}]
  (let [h (walk head)]
    (if (ast/evaluated? h)
      (walk (assoc app :head h))
      (assoc app :head h :tail (walk tail)))))

(def apply-rules
  {:I (simple apply-head)
   :A (simple apply-head)
   :F (simple apply-external)
   :μ apply-μ})

(defn apply [sexp]
  ((get apply-rules (ast/type (:head sexp)) apply-error) sexp))

;;;;; Eval

(defn walk-coll [xs]
  (into (ast/empty xs) (map walk) xs))

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
  (prevent-emission (update μ :body walk)))

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
