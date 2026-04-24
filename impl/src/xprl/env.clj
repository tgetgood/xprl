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
      (assoc-in [:bindings id] [env val])))

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

(defn deresolve [{:keys [env id sym] :as input}]
  (let [env (unbind env input)]
    ;; REVIEW: Does this really cause bugs? I'm not sure.
    ;; It certainly comes up. And if the value to which an input is bound were
    ;; to become captured by that same input, that would be nonsensical, so
    ;; guarding isn't crazy.
    (if (and (captured? env sym) (= id (capid env sym)))
      (uncapture env sym)
     env)))
