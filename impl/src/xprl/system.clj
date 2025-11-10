(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [trace!]]
            [xprl.env :as env]))

(def root-channels {})

(defn emit! [channels k v]
  (trace! "emitting" [k v] "on" (sort-by :names (keys channels)))
  (if (contains? channels k)
    ((get channels k) v) ; keep context on messages!
    ;; TODO: use the "unbound" channel if it exists to report these errors.
    ;; otherwise use the error channel if it exists
    ;; otherwise report to repl
    (println "message to unbound channel: " k v)))

(defn try-emissions!
  "Sends any messages that are ready to go, returns an emission containing the
  rest."
  [{:keys [kvs]} {:keys [ctx] :as env}]
  (ast/emission (loop [kvs kvs]
                  (if (seq kvs)
                    (let [[[k v] & more] kvs]
                      (if (or #_(ast/incomplete? v) (not (ast/keyword? k)))
                        kvs
                        (do
                          (emit! ctx k (env/attach-dyn v env))
                          (recur more))))
                    []))))
