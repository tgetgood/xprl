(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [trace!]]))

(def ret (ast/xkey :return))

(defn emit! [ctx [k v]]
  (trace! "emitting" [k v])
  (if-let [ch (get ctx k)]
    (ch v)
    (if-let [unbound (get ctx (ast/xkey :unbound))]
      (unbound [k v])
      (binding [*out* *err*]
        (println "message sent to unbound channel: " k v)))))

(defn try-emissions!
  "Sends any messages that are ready to go, returns an emission containing the
  rest."
  [{:keys [ctx] :as env} kvs]
  (trace! "trying emissions" kvs)
  ;; REVIEW: do we need to bundle the cc-map up with the Emission object?
  ;; What happens when an emission object gets returned (it's just data until
  ;; you walk it) or moved around by compiler optimisations? I haven't thought
  ;; this through any too well.
  ;;
  ;; Who says we can't emit an incomplete computation which can only be
  ;; completed in the receiving context?
  (if (or (:μ? env) (some (fn [[k v]] (not (ast/keyword? k))) kvs))
    (ast/emission kvs)
    (cond
      ;; TODO: Even when frozen we can and should perform non-channel returns
      ;; since they aren't really message passing.
      ;;
      ;; The problem is: what do we do with the *other* emissions?
      ;; It's just easier to wait until we can safely send them before returning
      ;; rets. So the above a potential optimisation, but is it necessary?
      ;; Put differently is there a case where the computation will stall if we
      ;; don't?
      ;; (:freeze? opts)            em
      (contains? ctx ret) (run! (partial emit! ctx) kvs)
      true
      (let [rets      (filter #(= ret (first %)) kvs)
            emissions (remove #(= ret (first %)) kvs)]
        (run! (partial emit! ctx) emissions)
        (when (> (count rets) 0)
          (when (> (count rets) 1)
            (println "Warning! multiple returns to non-stream location: " rets
                     "all but the first will be lost!"))
          (second (first rets)))))))
