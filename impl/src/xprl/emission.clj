(ns xprl.emission
  (:refer-clojure :exclude [bound?])
  (:require [xprl.ast :as ast])
    (:import [java.util WeakHashMap]))

(declare return)
(def ret (ast/xkey :return))

;;;;; Wires

(defn wire [& init]
  (let [w (ast/wire)]
    (when (seq init)
      (swap! (:state w) assoc :stream (vec init)))
    ;; REVIEW: We're just going to say the record itself is a write ref for now.
    w))

;;; Wire ops

;; FIXME: I'm far from convinced these are threadsafe. delivery should be
;; possible from multiple executors and reads should be as if it were immutable.
;; But of course it isn't under the hood and that complicates things so much...

(defn next-wire [w]
  (update w :offset inc))

(defn try-read! [env w]
  (let [state  @(:state w)
        offset (- (:offset w) (:offset state))]
    (assert (not (neg? offset)) "Trying to read freed stream segment!")
    (if (< offset (count (:stream state)))
      ;; if we have a value, return it
      (return env (nth (:stream state) offset))
      ;; otherwise park and wait
      (let [w' (update state :listeners update (:offset w) (fnil conj []) env)]
        (if (compare-and-set! (:state w) state w')
          nil
          ;; ::parked
          ;; spin!
          ;; REVIEW: I need these spinning cas ops for correctness, which
          ;; probably means atoms are the wrong primitive.
          (recur env w))))))

(defn drain-listeners! [wire offset value]
  (let [envs (get (:listeners @(:state wire)) offset)]
    (when (seq envs)
      (swap! (:state wire) update :listeners dissoc offset)
      (let [out (mapv (fn [e] [e value]) envs)]
        ;; FIXME: This doesn't really work because `return` is used in many
        ;; places other than `do-emission`.
        out))))

(defn deliver! [wire v]
  (let [state @(:state wire)
        next  (update state :stream conj v)]
    (if (compare-and-set! (:state wire) state next)
      (drain-listeners! wire (dec (+ (:offset next) (count (:stream next)))) v)
      (recur wire v))))

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

(defn captured? [cable]
  (::captured? cable))

;;;;; Sending messages

(defn send-captured! [env msgs]
  (let [ch (::capture-chan env)]
    (run! ch msgs)))

(defn send-1! [env [k v]]
  (if (contains? env k)
    (let [ch (get env k)]
      (cond
        (ast/wire? ch) (deliver! ch v)
        ;; fns always reach out of the program into the surrounding
        ;; implementation, so they can't return anything to the xprl runtime.
        (fn? ch)       (do (ch v) nil)
        true           (throw (RuntimeException.
                           (str "Bad channel type: " (type ch) " " ch)))))
    (do
      ;; TODO: :unbound channel
      ;; TODO: Keep errors in xprl.
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn return [env x]
  (send-1! env [ret x])
  nil)

(defn send! [env msgs]
  (into [] (comp (map (partial send-1! env)) cat) msgs))

(defn do-emission! [env msgs]
  (cond
    ;; Capture any local continuations with the emission; they will override
    ;; the cable in any future context of evaluation.
    ;; But the cable always gets to decide whether the context is cut,
    ;; captured, etc., so sandboxing should still work as expected.
    (cut? env)      (return env (ast/emission msgs))
    (captured? env) (send-captured! env msgs)
    true            (send! env msgs)))

;; general continuation manipulation

(defn with-return [env retfn]
  (assoc env ret retfn))

(defn ret-> {:style/indent [1]} [env inner outer]
  (inner (with-return env outer)))

(defn error! [env msg]
  ((get env (ast/xkey :error)) msg))
