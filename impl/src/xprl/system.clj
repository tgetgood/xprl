(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [trace!]]))

(def root-channels {})

(def ^:dynamic *ccmap* root-channels)

(defmacro with-ctx [channels body]
  `(binding [*ccmap* (merge *ccmap* ~channels)]
     (trace! "updated ccmap" *ccmap*)
     ~body))

(defn emit! [[k v]]
  (trace! "emitting" [k v])
  (if-let [ch (get *ccmap* k)]
    (ch v)
    (if-let [unbound (get *ccmap* (ast/xkeys :unbound))]
      (unbound [k v])
      (binding [*out* *err*]
        (println "message sent to unbound channel: " k v)))))

(defn try-emissions!
  "Sends any messages that are ready to go, returns an emission containing the
  rest."
  [{:keys [kvs] :as em}]
  (trace! "trying emissions" kvs)
  ;; Who says we can't emit an incomplete computation which can only be
  ;; completed in the receiving context?
  (if (some (fn [[k v]] (or (ast/incomplete? v) (not (ast/keyword? k)))) kvs)
    em                  ; Delay emissions until they're all ready.
    (run! emit! kvs)))
