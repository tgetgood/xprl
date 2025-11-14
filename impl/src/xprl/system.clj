(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [trace!]]
            [xprl.env :as env]))

(def root-channels {})

(defn emit! [env k v]
  (trace! "emitting" [k v])
  (if-let [ch (env/get-channel env k)]
    (ch v)
    (if-let [unbound (env/get-channel env (ast/xkeys :unbound))]
      (unbound [k v])
      (binding [*out* *err*]
        (println "message sent to unbound channel: " k v)))))

(defn try-emissions!
  "Sends any messages that are ready to go, returns an emission containing the
  rest."
  [{:keys [kvs]} env]
  (ast/emission (loop [kvs kvs]
                  (if (seq kvs)
                    (let [[[k v] & more] kvs]
                      (if (or #_(ast/incomplete? v) (not (ast/keyword? k)))
                        kvs
                        (do
                          (emit! env k (env/attach
                                        (env/merge-stacks (env/local v) env) v))
                          (recur more))))
                    []))))
