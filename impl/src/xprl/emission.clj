(ns xprl.emission
  (:refer-clojure :exclude [bound?])
  (:require [xprl.ast :as ast]
            [xprl.continuation :as cont]
            [xprl.executor :as exec])
    (:import [java.util WeakHashMap]))

;;;;; Wires

(defrecord Wire [id readers state])

(defn wire? [x]
  (instance? Wire x))

(defn new-wire []
  (->Wire (gensym "wire-") (WeakHashMap.) (atom {:listeners {}
                                                 :stream    []
                                                 :offset    0})))
(defrecord ReadRef [wire offset])

(defn read-ref [wire offset]
  (let [r (->ReadRef wire offset)]
    (.put (:readers wire) r offset)
    r))

(defn stream? [x]
  (instance? ReadRef x))

(defn wire [& init]
  (let [state (new-wire)]
    (when (seq init)
      (swap! (:state state) assoc :stream (vec init)))
    ;; REVIEW: We're just going to say the record itself is a write ref for now.
    [(read-ref state 0) state]))

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
  (dissoc cable ::cut? ::previous ::id cont/ret))

(defn captured? [cable]
  (::captured? cable))

;;;;; Wire ops

;; FIXME: I'm far from convinced these are threadsafe. delivery should be
;; possible from multiple executors and reads should be as if it were immutable.
;; But of course it isn't under the hood and that complicates things so much...

(defn next-stream [rr]
  (read-ref (:wire rr) (inc (:offset rr))))

(defn prune! [wire]
  (let [state     @(:state wire)
        minoffset (first (sort (.values (:readers wire))))]
    (when (< (:offset state) minoffset)
      (let [newstate (-> state
                         (assoc :offset minoffset)
                         (update :stream #(into [] (drop (- minoffset (:offset state)) %))))]
        (when-not (compare-and-set! (:state wire) state newstate)
          ;; spin!
          (recur wire))))))

(defn try-read! [env rr]
  (let [wire   @(:state (:wire rr))
        offset (- (:offset rr) (:offset wire))]
    (assert (not (neg? offset)) "Trying to read freed stream segment!")
    (if (< offset (count (:stream wire)))
      ;; if we have a value, return it
      (cont/return env (nth (:stream wire) offset))
      ;; otherwise park and wait
      (let [w' (update wire :listeners update (:offset rr) (fnil conj []) env)]
        (if (compare-and-set! (:state (:wire rr)) wire w')
          ::parked
          ;; spin!
          ;; REVIEW: I need these spinning cas ops for correctness, which
          ;; probably means atoms are the wrong primitive.
          (recur env rr))))))

(defn drain-listeners! [wire offset value]
  (let [envs (get (:listeners @(:state wire)) offset)]
    (when (seq envs)
      (swap! (:state wire) update :listeners dissoc offset)
      ;; N.B.: Don't eval these messages, just walk them, which will
      ;; automatically send them on to the appropriate return continuation from
      ;; the listener.
      (run! #(exec/enqueue! (fn [] (cont/return % value))) envs))))

(defn deliver! [wire v]
  (let [state @(:state wire)
        next  (update state :stream conj v)]
    (if (compare-and-set! (:state wire) state next)
      (drain-listeners! wire (+ (:offset next) (count (:stream next))) v)
      (recur wire v))))

;;;;; Sending messages

(defn send-captured! [env msgs]
  (let [ch (::capture-chan env)]
    (run! ch msgs)))

(defn send-1! [env [k v]]
  (if (contains? env k)
    (let [ch (get env k)]
      (cond
        (wire? ch) (deliver! ch v)
        (fn? ch)   (ch v)
        true       (throw (RuntimeException.
                           (str "Bad channel type: " (type ch) " " ch)))))
    (do
      ;; (println env)
      ;; TODO: :unbound channel
      ;; TODO: Keep errors in xprl.
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn send! [env msgs]
  (run! (partial send-1! env) msgs))

(defn do-emission! [env msgs]
  (cond
    ;; Capture any local continuations with the emission; they will override
    ;; the cable in any future context of evaluation.
    ;; But the cable always gets to decide whether the context is cut,
    ;; captured, etc., so sandboxing should still work as expected.
    (cut? env)      (cont/return env (ast/emission (clear env) msgs))
    (captured? env) (send-captured! env msgs)
    true            (send! env msgs)))
