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

(defn get-env [x]
  (::env (ast/ctx x)))

(defn lookup [env sym]
  (get-in env [:names (ast/free sym)]))

(defn symbols
  ([form] (symbols form (get-env form)))
  ([form e]
   (let [syms (ast/symbols form)]
     (into syms (comp (mapcat #(symbols (lookup e %))) (map ast/free)) syms))))

(defn project
  "Fits `env` by removing all names not mentioned in `form`."
  [env form]
  (let [syms (symbols form env)]
    (names empty-ns (into {} (filter #(contains? syms (ast/free (key %))))
                          (names env)))))

(defn with-env [x env]
  (ast/with-ctx x
    (assoc (ast/ctx x) ::env (project env x))))

(defn merge-env [outer inner]
  (names empty-ns (merge (names outer) (names inner))))

(defn carry-env [inner outer]
  (with-env inner (merge-env (get-env outer) (get-env inner))))

(defn bind [env s val]
  (assoc-in env [:names (ast/free s)] val))

(defn embedded-lookup [env sym]
  (let [ref (lookup env sym)]
    (with-env ref (merge-env env (get-env ref)))))

;;;;; env preserving ast traversal

(defn nearest-env [x k]
  (carry-env (get x k) x))

(defn params [x] (nearest-env x :params))
(defn name   [x] (nearest-env x :name))
(defn body   [x] (nearest-env x :body))
(defn head   [x] (nearest-env x :head))
(defn tail   [x] (nearest-env x :tail))
(defn form   [x] (nearest-env x :form))
(defn kvs    [x] (nearest-env x :kvs))

(defn xnth [x n]
  (carry-env (nth (:elements x) n) x))

(defn extract
  "Extracts all elements of List `xs` with their correctly inherited
  environments into a clj vector."
  [xs]
  (map #(xnth xs %) (range (count (:elements xs)))))

(defn map-list
  "Apply f to each element of xs, retaining the context."
  [f l]
  (carry-env (ast/list (map f (extract l))) l))

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
