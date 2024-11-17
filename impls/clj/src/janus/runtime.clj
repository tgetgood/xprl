(ns janus.runtime
  ;; FIXME: Minimise runtime requirements
  (:require [janus.ast :as ast])
  (:import [java.util.concurrent ConcurrentLinkedDeque]))

(declare ^:dynamic *coordinator* ^:dynamic *executor*)

(def JumpException
  "Special exception to unroll stack in event loop."
  (proxy [java.lang.RuntimeException]
      ["If you're seeing this, that's a problem!"]))

(defn top
  "Top level callback which just dumps its args into the void."
  [& args]
  ;; TODO: Real logging.
  (println ("Warning: value passed up to executor level.\nDropping: " args))
  (throw JumpException))

(defn steal! [system exec]
  (throw (RuntimeException. "not implemented.")))

(defn push! [^ConcurrentLinkedDeque exec tasks]
  (.addAll exec (reverse tasks))
  (throw JumpException))

(defn go! [^ConcurrentLinkedDeque exec]
  (try
    (if-let [task (.pollLast exec)]
      (apply (first task) (second task))
      (steal! *coordinator* exec))
    (catch RuntimeException e
      (when-not (identical? e JumpException)
        ;; TODO: Throwing here is wrong since it will shutdown the executor
        ;; and we don't want that. We need to log this as an error and
        ;; continue. Do we want to pause and recover?
        (throw e))))
  (recur exec))

(defn create-executor []
  ;; REVIEW: ConcurrentLinkedDeque is obviously too much overhead for the main
  ;; event loop of a real system. The design calls for singlethreaded dequeues
  ;; with a steal operation that needs to be somewhat coordinated. We'll get
  ;; there eventually.
  (ConcurrentLinkedDeque.))

(def ^:dynamic *coordinator* nil)
(def ^:dynamic *executor* (create-executor))

;;;;; Emission

(defn sanitise [k]
  (cond
    (ast/keyword? k) k
    (keyword k)      (ast/keyword (name k))
    :else            (throw (RuntimeException. "invalid continuation key."))))

(defn sanitise-keys [m]
  (into (empty m) (map (fn [[k v]] [(sanitise k) v])) m))

(defn unbound-channel [c]
  (fn [_ _ _]
    ;; TODO: Again, real logging.
    (println (str "WARNING: sending message on unbound channel: " c))))

(defn parse-emission [c [k v]]
  (let [k (if (keyword? k) (ast/keyword (name k)) k)]
    (cond
      (ast/keyword? k) [(get c k unbound-channel) v]
      ;; TODO: What kind of function?
      (fn? k)          [k v]
      :else            (throw (RuntimeException.
                               (str k " is not a valid channel."))))))

(defn emit
  "For each `kv`, sends `v` to continuation named by `k` in the continuation map
  `c` if `k` is a keyword. If `k` is a function it is assumed to *be* the
  continuation."
  [c & kvs]
  (push! *executor* (map (partial parse-emission c) (partition 2 kvs))))

(defn withcc
  ([c m]
   (merge c (sanitise-keys m)))
  ([c k v & kvs]
   (withcc c (apply hash-map k v kvs))))

;;;;; Collectors

(defn collector
  "Returns a collector which will collect `n` things and then call `next` with
  the result."
  [next n]
  ;; TODO: This is an array collector, but a map collector should work as a drop
  ;; in replacement. We just need to pass in the set of keys to be waited on.
  ;;
  ;; REVIEW: how ugly would it be to switch on whether `n` is an int or a
  ;; collection?
  (let [unset (gensym)
        fill  (take n (repeat unset))]
    (atom {:unset    unset
           :n        n
           :elements fill
           :next     next})))

(defn receive
  "Send the value `v` to the `i`th slot of collector c."
  [c i v]
  (swap! c
   (fn [{:keys [n unset elements] :as c}]
     (assert (and (> n 0) (= (get elements i) unset)) "Inconsistent collector.")
     (-> c
         (update :n dec)
         (update :elements assoc i v))))
  (when (= 0 (:n @c))
    (let [{:keys [next elements unset]} @c]
      (emit next :return elements))))

;;;;; dev entry

(defn pushngo! [f & args]
  (try
    (push! *executor* [[f args]])
    (catch Exception _
      (go! *executor*))))
