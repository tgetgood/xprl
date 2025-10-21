(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.system :as sys]))

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
    (if (:val sym)
      (:val sym)
      im)
    (throw (RuntimeException. (str "unbound symbol: " sym)))))

(defn eval [{form :form :as im}]
  (cond
    (ast/symbol? form)     (resolve im)
    ;; (I (P x y)) => (A (I x) y)
    (ast/pair? form)       (ast/application (ast/immediate (:head form)) (:tail form))
    ;; (I (L x y ...)) => (L (I x) (I y) ...)
    (vector? form)         (into [] (map ast/immediate) form)
    ;; (I {x y ...}) => {(I x) (I y) ...}
    ;; FIXME: maps are a pain in the ass because records are maps...
    ;; (map? form)            (into {} (map #(mapv ast/immediate %)) form)
    ;; (I V) => V. values are fixed points of eval.
    true                   form))

;;;;; Walk (previously `reduce`)

(defn walk [form env]
  (debug/trace! "--> " form)
  ;; (println (type form) form)
  (cond
    ;; REVIEW: These are repetitive but subtle. Is there anything to be gained
    ;; by hiding the complexity somewhere else?
    (ast/immediate? form)   (if (ast/incomplete? (:form form))
                              (let [form (update form :form walk env)]
                                (if (ast/incomplete? (:form form))
                                  form
                                  (eval form)))
                              (eval form))
    (ast/application? form) (if (ast/incomplete? (:head form))
                              (let [form (update form :head walk env)]
                                (if (ast/incomplete? (:head form))
                                  (update form :tail walk env)
                                  (apply form)))
                              (apply form))
    (ast/emission? form)    (let [form (update form :kvs walk env)]
                              (if (:μ? env) form (sys/emit (:ctx env) form)))

    (ast/keyword? form) form
    (ast/symbol? form)  form

    ;; TODO: I'll need a special node type for capture at this rate.
    (ast/ctx? form) (update form :form walk (update env :ctx merge (:chs form)))
    (ast/μ? form)   (update form :body walk (assoc env :μ? true))
    (vector? form)  (into [] (map #(walk % env)) form)
    (record? form)  (reduce (fn [acc [k v]] (assoc acc k (walk v env)))
                            form (ast/type-keys form))
    ;; (map? form) (reduce (fn [m [k v]] (assoc m (walk k env) (walk v env))) {} form)
    true            form))

;; Well... Is it too simple now?

(defn interpret [ns form]
  (loop [f (env/set-ns ns form)]
    (debug/trace! "step")
    (let [next (walk f {})] ; always restart with empty env. Stateless.
      (if (= f next)
        f
        (recur next)))))
