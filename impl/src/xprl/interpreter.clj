(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.meta :as meta]
   [xprl.system :as sys]))

(declare walk)

(defn walk-unevaled [form k check]
  (if (and (not (nil? check)) (ast/evaluated? (get form check)))
    form
    (update form k walk)))

;;;;; Application

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply [{:keys [head tail] :as form}]
  (cond
    (ast/incomplete? head) (update form :head walk) ; REVIEW: walk tail too?
    ;; FIXME: Walking the tail should be up to the extern.
    (ast/external? head)   (-> form (walk-unevaled :tail :tail) ((:fn head)))
    (ast/μ? head)          (walk (env/bind head tail)) ; REVIEW: do we need to walk here?
    true                   (apply-error form)))

;;;;; Eval

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

(defn eval [{form :form :as im}]
  (cond
    (ast/incomplete? form) (update im :form walk) ; REVIEW: walk tail too?
    ;; (I (P x y)) => (A (I x) y)
    (ast/pair? form)       (ast/application (ast/immediate (:head form)) (:tail form))
    ;; (I (L x y ...)) => (L (I x) (I y) ...)
    (ast/list? form)       (ast/list (map ast/immediate form)) ;REVIEW: why not vector?
    ;; (I {x y ...}) => {(I x) (I y) ...}
    (map? form)            (into {} (map #(mapv ast/immediate %)) form)
    (symbol? form)         (resolve im)
    ;; (I V) => V. values are fixed points of eval.
    true                   form))

;;;;; Reduction

(defn walk-ctx [{:keys [channels form] :as s}]
  (let [m    (update (meta s) :ctx merge channels)
        body (walk (with-meta form m))]
    (with-meta (assoc s :form body) (meta body))))

(defn walk [form]
  (cond
    (ast/immediate? form)   (eval form)
    (ast/application? form) (apply form)
    (ast/ctx? form)         (walk-ctx form)
    (ast/list? form)        (into (empty form) (map walk) form)
    (record? form)          (reduce (fn [acc k] (update acc k walk))
                                    form (ast/type-keys form))
    (map-entry? form)       [(walk (key form)) (walk (val form))]
    (map? form)             (into (empty form) (map walk) form)
    true                    form))

(defn interpret [ns form]
  (walk (ast/ctx sys/root-channels (env/ns-set! ns form))))
