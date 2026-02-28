(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(defn apply [env head tail]
  (cond
    (ast/μ? head) (walk (env/bind (env/merge-local env head) (:id head) (walk env tail))
                        (:body head))
    (ast/external? head)   (ast/call env head tail)
    (ast/incomplete? head) (ast/application env head (walk env tail))
    true                   (throw (RuntimeException. (str head " is not applicable!")))))

(defn resolve [env form]
  (let [env (env/merge-local env form)]
    ;; (println "resolve: " form (:bindings env))
    (cond
      (ast/input? form)  (if (env/bound? env form)
                           (env/binding env form)
                           (ast/immediate form))
      (ast/ref? form)    (:binding form)
      (ast/symbol? form) (ast/immediate form)
      true               (assert false "unreachable!!"))))

(defn eval [env form]
  (let [env (env/merge-local env form)]
    ;; (println "eval: " form  (:bindings env))
    (cond
      (ast/pair? form)       (apply env (walk env (ast/immediate (:head form))) (:tail form))
      (ast/symbolic? form)   (resolve env form)
      (ast/coll? form)       (into (ast/empty form) (map #(walk env (ast/immediate %))) form)
      (ast/incomplete? form) (ast/immediate form)
      true                   form)))

(defn walk [env form]
  (let [env (env/merge-local env form)]
    ;; (println "walk: " form (:bindings env))
    (cond
      (ast/immediate? form)   (eval env (walk env (:form form)))
      (ast/application? form) (apply env (walk env (:head form)) (:tail form))
      (ast/pair? form)        (env/with-env form env)
      (ast/input? form)       (env/with-env form env)
      (ast/μ? form)           (update form :body #(walk env %))
      (ast/coll? form)        (into (ast/empty form) (map (partial walk env)) form)
      (ast/emission? form)    (sys/try-emissions! env (walk env (:kvs form)))
      true                    form))) ; REVIEW: Do I need to merge envs?

(defn capture [form sym id]
  (cond
    (or (ast/application? form) (ast/pair? form))
    (-> form (update :head capture sym id) (update :tail capture sym id))
    ;; Use {} for input env since nothing can be bound before being captured.
    (ast/symbolic? form)  (if (= (ast/symbol form) sym)
                            (ast/input {} sym id)
                            form)
    (ast/immediate? form) (update form :form capture sym id)
    (ast/coll? form)      (into (ast/empty form) (map #(capture % sym id)) form)
    (ast/μ? form)         (if (= sym (:param form))
                            form
                            (update form :body capture sym id))
    true                  form))
