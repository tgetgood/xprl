(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [trace!]]
            [xprl.env :as env]))

(def root-channels {})

(def ^:dynamic *ccmap* root-channels)

(defmacro with-ctx [channels body]
  `(binding [*ccmap* (merge *ccmap* ~channels)]
     (trace! "updated ccmap" *ccmap*)
     ~body))

(defn emit! [k v]
  (trace! "emitting" [k v])
  (if-let [ch (get *ccmap* (env/strip k))]
    (ch v)
    (if-let [unbound (get *ccmap* (ast/xkeys :unbound))]
      (unbound [k v])
      (binding [*out* *err*]
        (println "message sent to unbound channel: " k v)))))

(defn try-emissions!
  "Sends any messages that are ready to go, returns an emission containing the
  rest."
  [{:keys [kvs]} _]
  (ast/emission (loop [kvs kvs]
                  (if (seq kvs)
                    (let [[[k v] & more] kvs]
                      (if (or (ast/incomplete? v) (not (ast/keyword? k)))
                        kvs
                        (do
                          (emit! k v)
                          (recur more))))
                    []))))
