(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.meta :as meta]
   [xprl.system :as sys]))

(declare walk)

(defn walk-key
  ([form k] (walk-key k nil))
  ([form k check]
   (if (and (not (nil? check)) (ast/evaluated? (get form check)))
     form
     (update form k walk))))

(defn walk-coll [form]
  (meta/meta-walk walk form))

;;;;; Application

(defn apply-μ [{μ :head tail :tail :as app}]
  (meta/wrap [update :cycle (fnil conj #{}) μ] (env/bind μ tail) walk))

(defn apply-external [{{f :fn} :head :as app}]
  (-> app (walk-key :tail :tail) f))

(defn apply-error [{app :form}]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [app]
  (-> app (walk-key :head) (walk-key :tail :head)))

(def apply-rules
  ;; Interpreter as middleware. Kind of a cool idea?
  {:I apply-head
   :A apply-head
   :F apply-external
   :μ apply-μ})

(defn apply [sexp]
  ((get apply-rules (ast/type (:head sexp)) apply-error) sexp))

;;;;; Eval

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-map [{m :form :as i}]
  (reduce (fn [m [k v]] (assoc m (assoc i :form k) (assoc i :form v))) (empty m) m))

(defn eval-pair [{{:keys [tail head]} :form}]
  (ast/application (ast/immediate head) tail))

(defn resolve [{sym :form :as im}]
  (cond
    (not (ast/resolved? sym))
    (throw (RuntimeException. (str ("unbound symbol: " sym))))

    (nil? (:val sym))
    (meta/mm im {:park {:cause :unresolved}})

    (contains? (:cycle (meta im)) (:val sym))
    (meta/mm im {:park {:cause :cycle}})

    true
    (with-meta (:val sym) (meta/clean (meta im)))))

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

(defn walk-μ [form]
  (meta/wrap [assoc :μ? true] form #(walk-key % :body)))

(defn walk-ctx [{:keys [channels form] :as s}]
  (let [m    (update (meta s) :ctx merge channels)
        body (walk (with-meta form m))]
    (with-meta (assoc s :form body) (meta body))))

(def walk-rules
  {:I eval
   :A apply
   :C walk-ctx
   :P walk-coll
   :L walk-coll
   :M walk-coll
   :μ walk-μ})

(defn walk* [sexp]
  (let [t     (ast/type sexp)
        next  ((get walk-rules t identity) sexp state)]
    (debug/trace! "walk:" t "\n" sexp "\n->\n" next)
    next))

;;;;; REVIEW: Debugging is still a pain with memoisation.

;; (def walk (memoize walk*))
(def walk walk*)

(def empty-state
  {:μ?    false
   :cycle #{}
   :ctx   {}})

(defn interpret [ns form]
  (walk (ast/ctx sys/root-channels (env/ns-set! ns form)) empty-state))
