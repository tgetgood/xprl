(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug :refer [deftracefn]]
   [xprl.env :as env]
   [xprl.system :as sys]))

(declare walk)

;;;;; Apply

(defn apply-error [app]
  (throw (RuntimeException. (str (:head app) " is not applicable!\n" app))))

(deftracefn apply [{:keys [head tail] :as form} env opts]
  (cond
    (ast/external? head) ((:fn head) form env opts) ; Punt to external interpreter.
    (ast/μ? head)        (walk (env/bindargs env head tail) env/empty-env opts)
    true                 (apply-error form)))

;;;;; Eval

(defn resolve [{sym :form :as im} env opts]
  (let [res (env/resolve env im)]
    (if (= res im)
      (if (ast/resolved? sym) ; lexical bindings shadow ns bindings!
        (:val sym)            ; has already been walked before making it into ns
        res)
      (walk res env/empty-env opts))))

(deftracefn eval [{form :form :as im} env opts]
  (if (ast/symbol? form)
    (resolve im env opts)
    (walk
      (cond
        ;; (I (P x y)) => (A (I x) y)
        (ast/pair? form) (ast/application (ast/immediate (:head form)) (:tail form))
        ;; (I (L x y ...)) => (L (I x) (I y) ...)
        (vector? form)   (into [] (map ast/immediate) form)
        ;; (I {x y ...}) => {(I x) (I y) ...}
        ;; FIXME: maps are a pain in the ass because records are maps...
        (ast/map? form)  (into {} (map #(mapv ast/immediate %)) form)
        ;; (I V) => V. values are fixed points of eval.
        true             form)
      env opts)))

;;;;; Walk (previously `reduce`)

(deftracefn walk [form env & [{:keys [freeze?] :as opts}]]
  (let [env (if-let [env (::env form)] env env)] ; <- this is awful..
    (cond
      (ast/immediate? form)   (if (ast/incomplete? (:form form))
                                (let [form (update form :form walk env opts)]
                                  (if (ast/incomplete? (:form form))
                                    form
                                    (eval form env opts)))
                                (eval form env opts))
      (ast/application? form) (if (ast/incomplete? (:head form))
                                (let [form (update form :head walk env opts)]
                                  (if (ast/incomplete? (:head form))
                                    (update form :tail walk env opts)
                                    (apply form env opts)))
                                (apply form env opts))
      (ast/emission? form)    (let [form (update form :kvs walk env opts)]
                                (if freeze?
                                  form
                                  (sys/try-emissions! (update form :kvs walk env) env)))

      (ast/ctx? form)  (sys/with-ctx (:chs form) (update form :form walk env opts))
      (ast/μ? form)    (update form :body walk env (assoc opts :freeze? true))
      (ast/pair? form) (-> form (update :head walk env opts) (update :tail walk env opts))
      (ast/list? form) (into [] (map #(walk % env opts)) form)
      (ast/map? form)  (into {} (map (fn [e] (mapv (fn [x] (walk x env opts)) e))) form)
      (= :error form)  (throw (RuntimeException. "fatal error"))
      true             form)))

;; Rewalk until fixed point. Is this really the best I can do?

(defn interpret [form]
  (debug/trace! "start")
  (walk form env/empty-env {}))
