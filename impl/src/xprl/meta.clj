(ns xprl.meta
  (:require [xprl.ast :as ast]))

(defn meta-walk [f xs]
  (with-meta
    (into (ast/empty xs)
          (comp (map #(with-meta % (meta xs))) (map f))
          xs)
    (meta xs)))

(defn wrap
  "Update `form`'s metadata, run `f`, then revert meta data in return value."
  [[kf k & args] form f]
  (let [o (f (with-meta form (apply kf (meta form) k args)))]
    (with-meta o (assoc (meta o) k (get (meta form) k)))))

(defn mm [form m]
  (with-meta form (merge (meta form) m)))

(defn clean [m]
  (dissoc m :park))
