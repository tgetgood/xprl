(ns xprl.emission
  (:refer-clojure :exclude [bound?])
  (:require [xprl.ast :as ast]))

;;;;; Dealing with return continuations

;; lots of special cases...

(def ret (ast/xkey :return))

(defn return {:style/indent 1} [env x]
  (let [retfn (get env ret)]
    (assert (fn? retfn) (str "Cannot return " x ". No destination."))
    ;; REVIEW: I skip the executor for local returns because it's simple and
    ;; I'm afraid of everything grinding to a halt if I don't. This could be
    ;; premature optimisation and I should look here first if there are weird
    ;; bugs.
    (retfn x)))

(defn with-return [env retfn]
  (assoc env ret retfn))

(defn ret-> {:style/indent [1]} [env inner outer]
  (inner (with-return env outer)))

;;;;; Splicing

(defn cut "Create a 'spliced cable' from given cable."
  [cable id]
  ;; TODO: What does this mean?
  {::cut?     true
   ;; REVIEW: I'm keeping the old cable due entirely to paranoia, aren't I?
   ;; It's an attack surface I'm better off without.
   ::previous cable
   ::id       id})

(defn cut? [cable]
  (::cut? cable))

(defn clear [cable]
  (dissoc cable ::cut? ::previous ::id ret))

(defn captured? [cable]
  (::captured? cable))

;;;;; Sending messages

(defn send-captured! [env msgs]
  (let [ch (::capture-chan env)]
    (run! ch msgs)))

(defn send-1! [env [k v]]
  (if (contains? env k)
    ((get env k) v)
    (do
      ;; (println env)
      ;; TODO: :unbound channel
      ;; TODO: Keep errors in xprl.
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn send! [env msgs]
  (run! (partial send-1! env) msgs))

(defn do-emission! [env prev msgs]
  (let [env (merge (:env prev) env)]
    ;; (println env msgs)
    (cond
      ;; Capture any local continuations with the emission; they will override
      ;; the cable in any future context of evaluation.
      ;; But the cable always gets to decide whether the context is cut,
      ;; captured, etc., so sandboxing should still work as expected.
      (cut? env)      (return env (ast/emission (clear env) msgs))
      (captured? env) (send-captured! env msgs)
      true            (send! env msgs))))
