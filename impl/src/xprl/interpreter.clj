(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.env :as env]
   [xprl.system :as sys]))

(declare walk)

(defn call
  "Invokes primitive `f` with args `t` in `env`."
  [env f t]
  ((:fn f) env f t))

(defn apply [env head tail]
  (cond
    (ast/μ? head) (walk (env/bind (env/merge-local env head) (:id head) (walk env tail))
                        (:body head))

    (ast/macro? head)      (call env head tail)
    (ast/external? head)   (call env head (walk env tail))
    (ast/incomplete? head) (ast/application env head (walk env tail))
    true                   (throw (RuntimeException. (str head " is not applicable!")))))

(defn resolve [env form]
  (let [env (env/merge-local env form)]
    (cond
      (ast/input? form)  (if (env/bound? env form)
                           (env/binding env form)

                           (ast/immediate form))
      (ast/ref? form)    (:binding form)
      (ast/symbol? form) (ast/immediate form)
      true               (assert false "unreachable!!"))))

(defn eval [env form]
  (let [env (env/merge-local env form)]
    ;; (println "eval: " form  (env/bindings env))
    (cond
      (ast/pair? form)       (apply env (walk env (ast/immediate (:head form))) (:tail form))
      (ast/symbolic? form)   (resolve env form)
      (ast/coll? form)       (into (empty form) #(walk env (ast/immediate %)) form)
      (ast/incomplete? form) (ast/immediate form)
      true                   form)))

(defn walk [env form]
  (let [env (env/merge-local env form)]
    ;; (println "walk: " form (env/bindings env))
    (cond
      (ast/immediate? form)   (eval env (walk env (:form form)))
      (ast/application? form) (apply env (walk env (:head form)) (:tail form))
      (ast/input? form)       (env/with-local form env)
      (ast/symbolic? form)    (let [sym (ast/symbol form)]
                                (if (env/captured? env form)
                                  (ast/input env sym (env/cap-sym env sym))
                                  form))
      (ast/μ? form)           (update form :body #(walk env %))
      (ast/coll? form)        (into (or (empty form) []) (map (partial walk env)) form)
      (ast/emission? form)    (sys/try-emissions! env (walk env (:kvs form)))
      true                    form))) ; REVIEW: Do I need to merge envs?
