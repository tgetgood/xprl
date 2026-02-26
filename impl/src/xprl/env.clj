(ns xprl.env
  (:require [clojure.set :as set]))

(defn env? [x]
  (and
   (instance? clojure.lang.IMeta x)
   (not (instance? xprl.ast.Symbol x))
   (not (instance? xprl.ast.Ref x))))

(defn get-env [x]
  (::env (meta x)))

(defn with-env [x env]
  (with-meta x (assoc (meta x) ::env env)))

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
  (if (env? x)
    (with-env x env)
    x))

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
