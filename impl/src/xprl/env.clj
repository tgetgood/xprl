(ns xprl.env
  (:refer-clojure :exclude [bound? binding])
  (:require [clojure.set :as set]
            [xprl.ast :as ast]))

(defn with-env [x env]
  (assoc x :env env))

(defn env-maps [x]
  (select-keys x [:bindings :ctx]))

(defn merge-envs [outer inner]
  (merge-with merge (env-maps outer) (env-maps inner)))

(defn merge-local [env x]
  (if (ast/env? x)
    (merge-envs env (:env x))
    env))

(defn bind [env id val]
  (-> env
      (assoc-in [:bindings id] val)))

(defn bound? [env input]
  (contains? (:bindings env) (:id input)))

(defn binding [env form]
  (get-in env [:bindings (:id form)]))

(defn unbind
  "Removes parameter bindings from nested invocations of the same function."
  [env μ]
  (update env :bindings dissoc (:id μ)))

(defn merge-ctx [env ctx]
  (update env :ctx merge ctx))

(defn ctx [env]
  (:ctx env))

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
