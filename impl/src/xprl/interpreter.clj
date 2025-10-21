(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]))

;;;;; Apply

(defn apply-error [app]
  (throw (RuntimeException.
          (str (:head app) " is not applicable, but was called with " (:tail app)
               "\n" (debug/provenance app)))))

(defn apply [{:keys [head tail] :as form}]
  (cond
    (ast/external? head)   ((:fn head) form) ; Punt to external interpreter.
    (ast/μ? head)          (env/bind head tail)
    true                   (apply-error form)))

;;;;; Eval

(defn resolve [{sym :form :as im}]
  (if (ast/resolved? sym)
    (:val sym)
    (throw (RuntimeException. (str "unbound symbol: " sym)))))

(defn eval [{form :form :as im}]
  (cond
    (ast/symbol? form)     (resolve im)
    ;; (I (P x y)) => (A (I x) y)
    (ast/pair? form)       (ast/application (ast/immediate (:head form)) (:tail form))
    ;; (I (L x y ...)) => (L (I x) (I y) ...)
    (vector? form)         (into [] (map ast/immediate) form)
    ;; (I {x y ...}) => {(I x) (I y) ...}
    (map? form)            (into {} (map #(mapv ast/immediate %)) form)
    ;; (I V) => V. values are fixed points of eval.
    true                   form))

;;;;; Walk (previously `reduce`)

(defn walk-ctx [{:keys [channels form] :as s}]
  s
  #_(let [m    (update (meta s) :ctx merge channels)
        body (walk (with-meta form m))]
    (with-meta (assoc s :form body) (meta body))))

(defn walk [form]
  (println "--> " form)
  (cond
    ;; REVIEW: These are repetitive but subtle. Is there anything to be gained
    ;; by hiding the complexity somewhere else?
    (ast/immediate? form)   (if (ast/incomplete? (:form form))
                              (let [form (update form :form walk)]
                                (if (ast/incomplete? (:form form))
                                  form
                                  (eval form)))
                              (eval form))
    (ast/application? form) (if (ast/incomplete? (:head form))
                              (let [form (update form :head walk)]
                                (if (ast/incomplete? (:head form))
                                  (update form :tail walk)
                                  (apply form)))
                              (apply form))

    (ast/ctx? form) (walk-ctx form)
    (vector? form)  (into [] (map walk) form)
    (record? form)  (reduce (fn [acc [k v]] (assoc acc k (walk v)))
                            form (ast/type-keys form))
    (map? form)     (reduce (fn [m [k v]] (assoc m (walk k) (walk v))) {} form)
    true            form))

;; Well... Is it too simple now?

(defn interpret [ns form]
  (loop [f (env/set-ns ns form)]
    (println "step")
    (let [next (walk f)]
      (if (= f next)
        f
        (recur next)))))
