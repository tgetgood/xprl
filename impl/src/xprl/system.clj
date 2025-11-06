(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [trace!]]))

(def root-channels {})

(defn emit! [channels k v]
  (trace! "emitting" [k v] "on" (sort-by :names (keys channels)))
  (if (contains? channels k)
    ((get channels k) v) ; keep context on messages!
    ;; TODO: use the "unbound" channel if it exists to report these errors.
    ;; otherwise use the error channel if it exists
    ;; otherwise report to repl
    (println "message to unbound channel: " k v)))
