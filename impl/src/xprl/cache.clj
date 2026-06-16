(ns xprl.cache
  (:import [java.util WeakHashMap]))

;; FIXME: This is NOT THREAD SAFE.
;; I ~think~ that the practical reprecussions in this instance amount to wasted
;; effort, but really I need to put a queue in front of this or something.
#_(defn weak-memo [f]
  (let [cache (WeakHashMap.)]
    (fn [& args]
      (if-let [v (.get cache args)]
        v
        (let [v (apply f args)]
          (.put cache args v)
          v)))))

;; REVIEW: I only need caching when I do dumb things, so I'm better off using it
;; as a red flag, yes?
(def weak-memo identity)
