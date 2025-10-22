(ns xprl.system
  (:require [xprl.ast :as ast]))

(def root-channels {})

(defn emit [chs {msgs :kvs :as e}]
  (run! (fn [[k v]]
          (assert (ast/keyword? k) (str (type k) k))
          (if (contains? chs k)
            ((get chs k) v)
            (println "message to unbound channel: " k v)))
        msgs))
