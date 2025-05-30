(ns janus.env
  (:refer-clojure :exclude [name])
  (:require
   [clojure.set :as set]
   [janus.ast :as ast]
   [janus.debug :refer [trace!]]))

(def empty-ns
  {:names {}})

(defn lookup [env sym]
  (get-in env [:names (ast/free sym)]))

(defn names
  ([env] (:names env))
  ([env xs] (assoc env :names xs)))

(defn project
  "Fits `env` by removing all names not mentioned in `form`."
  [env form]
  (let [syms (ast/symbols form)]
    (names empty-ns (into {} (filter #(contains? syms (ast/free (key %))))
                          (names env)))))

(defn get-env [x]
  (::env (ast/ctx x)))

(defn with-env [x env]
  (ast/with-ctx x
    (assoc (ast/ctx x) ::env (project env x))))

(defn bind [env s val]
  (assoc-in env [:names (ast/free s)] val))

(defn local-env [form]
  (let [env  (get-env form)
        syms (ast/symbols form)]
    (str "\n  env: "
         (into {} (filter #(contains? syms (key %))) (:names env)))))

;;;;; env preserving ast traversal

(defn merge-env [x y]
  (let [outer (or (get-env x) nil)
        inner (or (get-env y) outer)]
    (names inner (merge (names outer) (names inner)))))

(defn nearest-env [x k]
  (let [v (get x k)
        e (merge-env x v)]
    (trace! (ast/symbols v) e)
    (with-env v e)))

(defn params   [x] (nearest-env x :params))
(defn name     [x] (nearest-env x :name))
(defn body     [x] (nearest-env x :body))
(defn head     [x] (nearest-env x :head))
(defn tail     [x] (nearest-env x :tail))
(defn form     [x] (nearest-env x :form))
(defn kvs      [x] (nearest-env x :kvs))
(defn elements [x] (nearest-env x :elements))
(defn xnth   [x n] (nearest-env (elements x) n))

(defn map-list
  "Apply f to each element of xs, retaining the context."
  [f l]
  (ast/list (reduce (fn [acc x]
                      (conj acc (f (with-env x (merge-env l x)))))
                    [] (:elements l))))

;;;;; μ specfics

(defn μ-bind [μ args]
  (let [body (body μ)]
    (if-not (ast/contextual? body)
      (do
        (trace! "Primitive μ body detected, not binding params.")
        body)
      (let [env (bind (get-env body) (params μ) args)
            env (if (name μ) (bind env (name μ) μ) env)]
        (trace! "binding params:" (params μ) "to" args "in" body (local-env args))
        (with-env body env)))))
