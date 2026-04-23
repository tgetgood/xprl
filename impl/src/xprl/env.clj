(ns xprl.env
  (:refer-clojure :exclude [bound? binding])
  (:require [clojure.set :as set]
            [xprl.ast :as ast]))

(defn with-env [env x]
  (if (ast/env? x)
    (assoc x :env env)
    x))

(defn merge-envs [outer inner]
  (merge-with merge outer inner))

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

(defn capture [s sym id]
  (update s :captured assoc sym id))

(defn captured? [s sym]
  (contains? (:captured s) sym))

(defn capid [s sym]
  (get-in s [:captured sym]))

(defn uncapture [s sym]
  (update s :captured dissoc sym))

(defn deresolve [env input]
  (let [syms (filter (fn [[k v]] (= v (:id input))) (:captured env))]
    (assert (< (count syms) 2) (vec syms))
    (-> (:env input)
        (unbind input)
        (uncapture (first (first syms))))))
