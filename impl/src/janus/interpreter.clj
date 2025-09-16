(ns janus.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug]
   [janus.env :as env]
   [janus.system :as sys]))

(declare walk)

(defn walk-in [state form]
  (:form (walk (assoc state :form form))))

(defn xform
  "Wraps a function that just acts on a form to act on the form embedded in a
  state map."
  [f]
  (fn [next]
    (fn [state]
      (next (update state :form f)))))

(defn tap [f]
  (fn [state]
    (println state)
    (f state)))

(defn walk-key [k]
  (fn [next]
    (fn [state]
      (next (update-in state [:form k] #(walk-in state %))))))

(defn walk-key-when-unev
  "If `(get state [:form ekey])` is unevaluated, then walk `wkey`. Otherwise do
  nothing."
  [wkey ekey]
  (fn [next]
    (fn [state]
      (if (ast/evaluated? (get-in state [:form ekey]))
        (next state)
        (next ((walk-key wkey) state))))))

(defn walk-coll [next]
  (fn [{xs :form :as state}]
    (let [ys (into (ast/empty xs)
                   (comp (map #(assoc state :form %)) (map walk) (map :form))
                   xs)]
      (next (assoc state :form ys)))))

;;;;; Application

(defn with-ctx [next]
  (fn [{{μ :head} :form :as state}]
    (-> state
        (update :cycle (fnil conj #{}) μ)
        next
        (assoc :cycle (:cycle state)))))

(defn apply-μ [{μ :head tail :tail :as app}]
  (env/bind μ tail))

(defn apply-external [{{f :fn} :head :as app}]
  (f app))

(defn apply-error [{app :form}]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(def apply-head
  (comp (walk-key-when-unev :tail :head) (walk-key :head)))

(def apply-rules
  ;; Interpreter as middleware. Kind of a cool idea?
  {:I apply-head
   :A apply-head
   :F (comp (xform apply-external) (walk-key-when-unev :tail :tail))
   :μ (comp (xform apply-μ) with-ctx)})

(defn apply [sexp]
  ((get apply-rules (ast/type (:head sexp)) apply-error) sexp))

;;;;; Eval

(defn eval-list [im]
  (ast/list (map ast/immediate (:form im))))

(defn eval-map [{m :form :as i}]
  (reduce (fn [m [k v]] (assoc m (assoc i :form k) (assoc i :form v))) (empty m) m))

(defn eval-pair [{{:keys [tail head]} :form}]
  (ast/application (ast/immediate head) tail))

(defn resolve [next]
  (fn [{sym :form cycle :cycle :as state}]
    (cond
      (not (ast/resolved? sym))    (next state)
      (contains? cycle (:val sym)) (next state)
      true                         (next (update state :form :val)))))

(def eval-rules
  {:P (xform eval-pair)     ; (I (P x y)) => (A (I x) y)
   :L (xform eval-list)     ; (I (L x y ...)) => (L (I x) (I y) ...)
   :M (xform eval-map)      ; (I {x y ...}) => {(I x) (I y) ...}
   :I walk-coll
   :A walk-coll
   :V (xform :form)         ; (I V) => V. values are fixed points of eval.
   :S resolve})

(defn eval [sexp]
  ((get eval-rules (ast/type (:form sexp))) sexp))

;;;;; Reduction

(defn walk-μ [next]
  (fn [state]
    (-> state
        (assoc :μ? true)
        (walk-key :body)
        (assoc :μ? (:μ? state))
        next)))

(defn walk-ctx [next]
  (fn [{{:keys [channels form]} :form :as state}]
    (let [s' (-> state
                 (update :ctx merge channels)
                 (assoc :form form)
                 walk)]
      ;; TODO: Index parked computations
      (next (update state :form assoc :form (:form s'))))))

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
        next  ((get walk-rules t identity) sexp)]
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
  (walk-in empty-state (ast/ctx sys/root-channels (env/ns-set! ns form))))
