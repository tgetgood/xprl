(ns xprl.env
  (:refer-clojure :exclude [bound? binding])
  (:require [clojure.set :as set]
            [xprl.ast :as ast]))

(defn with-env [x env]
  (assoc x :env env))

(defn env-maps [x]
  (select-keys x [:bindings :ctx]))

(defn merge-envs [outer inner]
  (-> (merge-with merge (env-maps outer) (env-maps inner))
      (assoc :μ? (or (:μ? outer) (:μ? inner)))))

(defn merge-local [env x]
  (if (ast/env? x)
    (merge-envs env (:env x))
    env))

(defn bind [env id val]
  (-> env
      (assoc-in [:bindings id] val)
      (assoc :μ? false)))

(defn bound? [env input]
  (contains? (:bindings env) (:id input)))

(defn binding [env form]
  (get-in env [:bindings (:id form)]))

(defn merge-ctx [env ctx]
  (update env :ctx merge ctx))

(defn ctx [env]
  (:ctx env))

(defn bindings
  "Returns the current effective bindings of an env. For debug output."
  [env]
  (into {} (map (fn [[k v]] [k (get-in env [:bindings v])])) (:captured env)))
