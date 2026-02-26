(ns xprl.env
  (:refer-clojure :exclude [bound? binding])
  (:require [clojure.set :as set]))

(defn env? [x]
  (instance? clojure.lang.IMeta x))

(defn get-env [x]
  (::env (meta x)))

(def persistent-keys
  "Env keys which should always be persisted."
  [:bindings :ctx :μ?])

(defn with-transient-env [x env]
  (with-meta x (assoc (meta x) ::env env)))

(defn with-env [x env]
  (with-transient-env x env)
  ;; (with-meta x (assoc (meta x) ::env (select-keys env persistent-keys)))
  )

(defn env-maps [x]
  (select-keys x [:captured :bindings :ctx]))

(defn merge-envs [outer inner]
  (-> (merge-with merge (env-maps outer) (env-maps inner))
      (assoc :μ? (or (:μ? outer) (:μ? inner)))))

(defn merge-local [env x]
  (if (env? x)
    (merge-envs env (get-env x))
    env))

(defn with-local [x env]
  (with-env x env))

(defn capture [env sym id]
  (-> env
      (assoc-in [:captured sym] id)
      (assoc :μ? true)))

(defn captured? [env sym]
  (contains? (:captured env) sym))

(defn cap-sym [env sym]
  (get-in env [:captured sym]))

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
