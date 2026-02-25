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

(defn purge [x poisoned]
  (into {} (remove (fn [[k v]] (contains? (get poisoned k) v))) x))

(defn poison [sym id]
  {:poison {sym #{id}}})

(defn merge-envs [outer inner]
  (let [cs (merge (:captured outer) (:captured inner))
        po (merge-with set/union (:poison outer) (:poison inner))]
    {:captured (purge cs po)
     :bindings (merge (:bindings outer) (:bindings inner))
     :ctx      (merge (:ctx outer) (:ctx inner))
     :posion   po
     :μ?       (or (:μ? outer) (:μ? inner))}))

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
  (and (contains? (:captured env) sym)
       (not (contains? (:bindings env) sym))))

(defn bind [env id val]
  (-> env
      (assoc-in [:bindings id] val)
      (assoc :μ? false)))

(defn bindings
  "Returns the current effective bindings of an env. For debug output."
  [env]
  (into {} (map (fn [[k v]] [k (get-in env [:bindings v])])) (:captured env)))
