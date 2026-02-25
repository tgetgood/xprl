(ns xprl.env
  (:require [xprl.ast :as ast :refer [Env]]))

(defn merge-local-env [env x]
  (if (satisfies? xprl.ast.Env x)
    (merge-with merge env (:env x))
    env))

(defn with-local [env x]
  (if (satisfies? xprl.ast.Env x)
    (assoc x :env env)
    x))
