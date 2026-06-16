(ns xprl.cache
  (:import [java.util WeakHashMap]))

;; FIXME: This is NOT THREAD SAFE.
;; I ~think~ that the practical reprecussions in this instance amount to wasted
;; effort, but really I need to put a queue in front of this or something.
(defn weak-memo [f]
  (let [cache (WeakHashMap.)]
    (fn [& args]
      (if-let [v (.get cache args)]
        v
        (let [v (apply f args)]
          (.put cache args v)
          v)))))
