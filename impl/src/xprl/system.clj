(ns xprl.system
  (:require [xprl.ast :as ast]))

(def root-channels {})

(defn emit [chs {msgs :kvs :as e}]
  (println "emit")
  (loop [[[k v] & more] msgs]
    (println k (get chs k))
    (assert (ast/keyword? k) (str (type k) k))
    (if (contains? chs k)
      ((get chs k) v)
      (println "message to unbound channel: " k v))))
