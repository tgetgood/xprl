(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [trace!]]))

(def root-channels {})

(def ^:dynamic *ccmap* root-channels)

(def ret (ast/xkeys :return))

(defmacro with-ctx [channels body]
  `(binding [*ccmap* (merge *ccmap* ~channels)]
     (trace! "updated ccmap" *ccmap*)
     ~body))

(defmacro walk-ctx [form body]
  {:style/indent 1}
  `(with-ctx (:chs ~form)
     (if (or (ast/incomplete? (:form ~form))
             (ast/emission? (:form ~form))
             (not (contains? *ccmap* ret)))
       ~body
       (let [v# (:form ~form)]
         (when-not (nil? v#) ; nil is not a message. It signifies the lack of one!
           ((get *ccmap* ret) v#))))))

(defmacro with-return-ctx [opts body]
  `(if (:return-ctx? ~opts)
     (let [~opts (dissoc ~opts :return-ctx?)]
       ~body)
     (binding [*ccmap* (dissoc *ccmap* ret)]
       ~body)))

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
  [{:keys [kvs] :as em} opts]
  (trace! "trying emissions" kvs)
  ;; Who says we can't emit an incomplete computation which can only be
  ;; completed in the receiving context?
  (if (some (fn [[k v]] (or (ast/incomplete? v) (not (ast/keyword? k)))) kvs)
    em                  ; Delay emissions until they're all ready.
    (cond
      ;; TODO: Even when frozen we can and should perform non-channel returns
      ;; since they aren't really message passing.
      ;;
      ;; The problem is: what do we do with the *other* emissions?
      ;; It's just easier to wait until we can safely send them before returning
      ;; rets. So is the ~should~ above really true?
      (:freeze? opts)         em
      (contains? *ccmap* ret) (run! emit! kvs)
      true
      (let [rets      (filter #(= ret (first %)) kvs)
            emissions (remove #(= ret (first %)) kvs)]
        (run! emit! emissions)
        (when (> (count rets) 0)
          (when (> (count rets) 1)
            (println "Warning! multiple returns to non-stream location: " rets
                     "all but the first will be lost!"))
          (second (first rets)))))))
