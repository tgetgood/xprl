(ns janus.runtime
  ;; FIXME: Minimise runtime requirements
  (:require [janus.ast :as ast]
            [taoensso.telemere :as t])
  (:import [java.util.concurrent ConcurrentLinkedDeque]))

;;;;; Standard keys

;; TODO: Qualify these keys.
(def return (ast/keyword "return"))
(def error (ast/keyword "error"))
(def env (ast/keyword "env"))

;;;;;

(declare ^:dynamic *coordinator* ^:dynamic *executor*)

(def JumpException
  "Special exception to unroll stack in event loop."
  (proxy [java.lang.RuntimeException]
      ["If you're seeing this, that's a problem!"]))

(defn top
  "Top level callback which just dumps its args into the void."
  [& args]
  (t/log! :warn ["value passed up to executor level. Dropping:" args])
  (throw JumpException))

(defn steal! [system exec]
  ;; REVIEW: What to log here? Executors ought to have names.
  (t/event! ::work-steal {:level :trace})
  ;; TODO: steal work!
  (t/log! :info "Work queue empty. Nothing to do")
  #_(throw (RuntimeException. "stealing not implemented.")))

(defn push! [^ConcurrentLinkedDeque exec tasks]
  (t/event! ::push-tasks {:data  {:count (count tasks)
                                  :tasks tasks}
                          :level :trace})
  ;; FIXME: When there's only one task, this amounts to
  ;; 1) push task onto stack
  ;; 2) run around the pond
  ;; 3) pop task off stack
  ;; 4) execute task
  ;;
  ;; Steps 1-3 are unnecessary in the one task case. In fact, we should always
  ;; push all but one and directly step into the last one.
  ;;
  ;; However, under the current implementation of recursion that will lead to
  ;; unbounded stack growth on the jvm, which will then blow up.
  ;;
  ;; I'm not entirely sure what to do about that. My current solution is to
  ;; ignore it until it becomes a problem and hopefully we can bootstrap the
  ;; language to assembly before it does... but we'll see.
  (.addAll exec (reverse tasks))
  (throw JumpException))

(defn go! [^ConcurrentLinkedDeque exec]
  (try
    (if-let [task (.pollLast exec)]
      (do
        (t/event! ::pop-task {:data task :level :trace})
        ((first task) (second task)))
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
  (when-not (ast/keyword? k)
    (t/log! {:level :error :data [(type k) k]}
            "Only xprl keywords can be used in cc maps.")
    (throw (RuntimeException. "")))
  k)

(defn sanitise-keys [m]
  (into (empty m) (map (fn [[k v]] [(sanitise k) v])) m))

(defn unbound-channel [c]
  (fn [_ _ _]
    (t/log! :warn ["message sent on unbound channel:" c])))

(defn parse-emission [c [k v]]
  (t/log! :debug ["Emission:" k (get c k) v])
  (cond
    (ast/keyword? k) [(get c k unbound-channel) v]
    ;; TODO: What kind of function?
    (fn? k)          [k v]
    :else            (do
                       (t/log! {:level :error
                                :data [(type k) k]}
                               "Only fns and xprl keywords can be channels.")
                       (throw (RuntimeException.
                               (str k " is not a valid channel."))))))

(defn emit
  "For each `kv`, sends `v` to continuation named by `k` in the continuation map
  `c` if `k` is a keyword. If `k` is a function it is assumed to *be* the
  continuation."
  {:style/indent 1}
  [c & kvs]
  (t/log! :trace ["emit raw" kvs])
  (let [tasks (map (partial parse-emission c) (partition 2 kvs))]
    (push! *executor* tasks)))

(defn withcc
  {:style/indent 1}
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
        fill  (into [] (take n) (repeat unset))]
    (atom {:unset    unset
           :n        n
           :elements fill
           :next     next})))

(defn receive
  "Send the value `v` to the `i`th slot of collector c."
  [c i v]
  (t/event! :collector/receive {:level :trace :data [i v]})
  (swap! c
   (fn [{:keys [n unset elements] :as c}]
     (assert (and (> n 0) (= (get elements i) unset)) "Inconsistent collector.")
     (-> c
         (update :n dec)
         (update :elements assoc i v))))
  (when (= 0 (:n @c))
    (let [{:keys [next elements unset]} @c]
      (t/event! :collector/join {:level :trace :data elements})
      (emit next return elements))))

;;;;; dev entry

(defn pushngo! [& forms]
  (let [tasks (mapv (fn [x] [#(apply (first x) %) (vec (rest x))]) forms)]
    (try
      (push! *executor* tasks)
      (catch Exception _
        (go! *executor*)))))
