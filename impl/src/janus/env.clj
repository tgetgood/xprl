(ns janus.env
  (:refer-clojure :exclude [name])
  (:require
   [clojure.set :as set]
   [janus.ast :as ast]
   [janus.debug :refer [trace!]]))

(def empty-ns
  {:names {}})

(defn names
  ([env] (or (:names env) {}))
  ([env xs] (assoc env :names xs)))

(defn clear [env sym]
  (update env names dissoc sym))

#_(defn get-env [x]
  (::env (ast/ctx x)))

(defn lookup [env sym]
  (get-in env [:names sym]))

(defn symbols
  ([form] (symbols form (get-env form)))
  ([form e]
   (let [syms (ast/symbols form)]
     (into syms (mapcat #(symbols (lookup e %))) syms))))

(defn project
  "Fits `env` by removing all names not mentioned in `form`."
  [env form]
  (let [syms (symbols form env)]
    (names empty-ns (into {} (filter #(contains? syms (key %)))
                          (names env)))))

#_(defn with-env [x env]
  (ast/with-ctx x
    (assoc (ast/ctx x) ::env (project env x))))

(defn merge-env [outer inner]
  (names empty-ns (merge (names outer) (names inner))))

(defn bind [env s val]
  (assoc-in env [:names s] val))

(defn embedded-lookup [env sym]
  (let [ref (lookup env sym)]
    (with-env ref (merge-env env (get-env ref)))))

;;;;; μ specfics

(defn μ-bind [μ args]
  (let [body (body μ)]
    (if-not (ast/contextual? body)
      (do
        (trace! "Primitive μ body detected, not binding params.")
        body)
      (let [env (bind (get-env body) (params μ) args)
            env (if (name μ) (bind env (name μ) μ) env)]
        (trace! "binding params:" (params μ) "to" args "in" body)
        (with-env body env)))))
