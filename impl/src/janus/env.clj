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

(declare with-env)

(defn lookup [env sym]
  (let [ref (get-in env [:names (ast/free sym)])]
    (with-env ref
      (names empty-ns
             (merge (names env) (names (get-env ref)))))))

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

(defn bind [env s val]
  (assoc-in env [:names (ast/free s)] val))

;;;;; env preserving ast traversal

(defn merge-env [outer inner]
  (names empty-ns (merge (names (get-env outer)) (names (get-env inner)))))

(defn nearest-env [x k]
  (let [v (get x k)
        e (merge-env x v)]
    (with-env v e)))

(defn params [x] (nearest-env x :params))
(defn name   [x] (nearest-env x :name))
(defn body   [x] (nearest-env x :body))
(defn head   [x] (nearest-env x :head))
(defn tail   [x] (nearest-env x :tail))
(defn form   [x] (nearest-env x :form))
(defn kvs    [x] (nearest-env x :kvs))

(defn xnth [x n]
  (let [el (nth (:elements x) n)]
    (with-env el (merge-env x el))))

(defn extract
  "Extracts all elements of List `xs` with their correctly inherited
  environments into a clj vector."
  [xs]
  (map #(xnth xs %) (range (count (:elements xs)))))

(defn map-list
  "Apply f to each element of xs, retaining the context."
  [f l]
  (ast/list (map f (extract l))))

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
