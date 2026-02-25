(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.env :as env]
   [xprl.system :as sys]))

(declare walk)

(defn bind [env id val]
  (-> env
      (assoc-in [:bindings id] val)
      (assoc :μ? false)))

(defn resolve [env form]
  (let [env (env/merge-local-env env form)]
    (cond
      (ast/input? form)  (if (contains? (:bindings env) (:id form))
                           (get-in env [:bindings (:id form)])
                           (ast/immediate form))
      (ast/ref? form)    (:binding form)
      (ast/symbol? form) (throw (RuntimeException. (str "unbound symbol: " form)))
      true               (assert false "unreachable!!"))))

(defn call [env f t]
  ((:fn f) env f t))

(defn apply [env head tail]
  (let [env (env/merge-local-env env head)]
    (cond
      (ast/μ? head)          (walk (bind env (:id head) (walk env tail)) (:body head))
      (ast/macro? head)      (call env head tail)
      (ast/external? head)   (call env head (walk env tail))
      (ast/incomplete? head) (ast/application env head (walk env tail))
      true                   (throw (RuntimeException. (str head " is not applicable!"))))))

(defn eval [env form]
  (let [env (env/merge-local-env env form)]
    (cond
      (ast/pair? form)       (apply env (walk env (ast/immediate (:head form))) (:tail form))
      (ast/symbolic? form)   (resolve env form)
      (ast/coll? form)       (into (empty form) (map (partial eval env)) form)
      (ast/incomplete? form) (ast/immediate form)
      true                   form)))

(defn walk [env form]
  (let [env (env/merge-local-env env form)]
    (cond
      (ast/immediate? form)   (eval env (walk env (:form form)))
      (ast/application? form) (apply env (walk env (:head form)) (:tail form))
      (ast/symbolic? form)    (let [sym (ast/symbol form)]
                                (if (contains? (:captured env) sym)
                                  (ast/input env sym (get-in env [:captured sym]))
                                  (env/with-local env form)))
      (ast/μ? form)           (update form :body #(walk env %))
      (ast/coll? form)        (into (empty form) (map (partial walk env)) form)
      true                    form))) ; REVIEW: Do I need to merge envs?


;; FIXME: These are the two cases I've dropped from the previous interpreter impl:
;;
;; (ast/emission? form) (sys/try-emissions! (update form :kvs walk opts) opts)
;; (ast/ctx? form)      (sys/walk-ctx form
;;                        (update form :form walk (assoc opts :return-ctx? true)))
;;
;; They're not going to work without rewriting, but I'll keep them around for
;; the reference until I get to it.
