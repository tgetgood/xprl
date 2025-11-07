(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug :refer [deftracefn]]
   [xprl.env :as env]
   [xprl.system :as sys]))

;;;;; Apply

(defn apply-error [app]
  (throw (RuntimeException. (str (:head app) " is not applicable!\n" app))))

(deftracefn apply [{:keys [head tail] :as form} env]
  (cond
    (ast/external? head) ((:fn head) form env) ; Punt to external interpreter.
    (ast/μ? head)        (env/μ-binding head (env/anchor env tail))
    true                 (apply-error form)))

;;;;; Eval

(defn resolve [{sym :form :as im} env]
  (cond
    (env/bound? env sym) (env/resolve env sym)
    (ast/resolved? sym)  (:val sym)
    true                 im))

(deftracefn eval [{form :form :as im} env]
  (cond
    (ast/symbol? form) (resolve im env)
    ;; (I (B X)) => (B (I X))
    (ast/lex? form)    (update form :form ast/immediate)
    ;; (I (P x y)) => (A (I x) y)
    (ast/pair? form)   (ast/application (ast/immediate (:head form)) (:tail form))
    ;; (I (L x y ...)) => (L (I x) (I y) ...)
    (vector? form)     (into [] (map ast/immediate) form)
    ;; (I {x y ...}) => {(I x) (I y) ...}
    ;; FIXME: maps are a pain in the ass because records are maps...
    (ast/map? form)    (into {} (map #(mapv ast/immediate %)) form)
    ;; (I V) => V. values are fixed points of eval.
    true               form))

;;;;; Walk (previously `reduce`)

(deftracefn walk [form env]
  (cond
    (ast/immediate? form)   (if (ast/incomplete? (:form form))
                              (let [form (update form :form walk env)]
                                (if (ast/incomplete? (:form form))
                                  form
                                  (eval form env)))
                              (eval form env))
    (ast/application? form) (if (ast/incomplete? (:head form))
                              (let [form (update form :head walk env)]
                                (if (ast/incomplete? (:head form))
                                  (update form :tail walk env)
                                  (apply form env)))
                              (apply form env))
    (ast/emission? form)    (let [form (update form :kvs walk env)]
                              (if (:μ? env)
                                form
                                (sys/try-emissions! (update form :kvs walk env) env)))

    (ast/lex? form)  (env/compact (update form :form walk (env/incorporate env form)))
    (ast/ctx? form)  (update form :form walk (env/walk-channels env form))
    (ast/μ? form)    (update form :body walk (env/walk-μ env form))
    (ast/pair? form) (-> form (update :head walk env) (update :tail walk env))
    (ast/list? form) (into [] (map #(walk % env)) form)
    (ast/map? form)  (into {} (map (fn [e] (mapv (fn [x] (walk x env)) e))) form)
    (= :error form)  (throw (RuntimeException. "fatal error"))
    true             form))

;; Well... Is it too simple now?

(defn interpret [form]
  (loop [f form]
    (debug/trace! "step")
    (let [next    (walk f env/empty-env)]
      (if (= f next)
        f
        (recur next)))))
