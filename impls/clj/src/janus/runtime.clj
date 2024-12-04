(ns janus.runtime
  (:refer-clojure :exclude [run!])
  (:require [janus.ast :as ast]
            ;; FIXME: Minimise runtime requirements
            [taoensso.telemere :as t])
  (:import [java.util.concurrent ConcurrentLinkedDeque]))

;;;;; Standard keys

;; TODO: Qualify these keys.
(def return (ast/keyword "return"))
(def error (ast/keyword "error"))
(def env (ast/keyword "env"))

;;;;;

;; FIXME: next and running? should be volative mutables, but I don't want to
;; deal with that until performance actually becomes a problem.
;;
;; Remember: an executor is bound to a single thread, so concurrency isn't an
;; issue except during work stealing.
(defrecord Executor [^ConcurrentLinkedDeque queue next running?])

(defn create-executor []
  ;; REVIEW: ConcurrentLinkedDeque is obviously too much overhead for the main
  ;; event loop of a real system. The design calls for singlethreaded dequeues
  ;; with a steal operation that needs to be somewhat coordinated. We'll get
  ;; there eventually.
  (->Executor (ConcurrentLinkedDeque.) (atom nil) (atom false)))

(def ^:dynamic *coordinator* nil)
(def ^:dynamic *executor* (create-executor))

(def JumpException
  "Special exception to unroll stack in event loop."
  (proxy [java.lang.RuntimeException]
      ["If you're seeing this, that's a problem!"]))

(defn steal! [system exec]
  ;; REVIEW: What to log here? Executors ought to have names.
  (t/event! ::work-steal {:level :trace})
  ;; TODO: steal work!
  (t/log! :debug "Work queue empty. Nothing to do")
  nil)

(declare go!)

(defn push!
  [^Executor exec tasks]
  (t/event! ::push-tasks {:data  {:count (count tasks)
                                  :tasks tasks}
                          :level :trace})
  (when (seq tasks)
    (if @(:next exec)
      (.addAll (:queue exec) (reverse tasks))
      (do
        (when (< 1 (count tasks))
          (.addAll (:queue exec) (reverse (rest tasks))))
        (compare-and-set! (:next exec) nil (first tasks)))))
  (if @(:running? exec)
    (throw JumpException)
    (do
      (reset! (:running? exec) true)
      (go! exec))))

(defn run! [task]
  (t/event! ::run {:level :trace :data {:fn (first task) :args (rest task)}})
  ((first task) (rest task))
  ;; FIXME: As currently implemented, the stack will grow until some task emits
  ;; nothing.
  ;;
  ;; A task emitting nothing means that a thread of execution has died.
  ;;
  ;; Presumably the value it was working on was either discarded or aggregated
  ;; into something else.
  ;;
  ;; This isn't so different from normal stack behaviour, but I'm not optimising
  ;; tail calls, so this might well blow up.
  (throw JumpException))

(defmacro check-jump
  {:style/indent 0}
  [& body]
  `(try
     ~@body
     (catch RuntimeException e#
       (if (identical? e# JumpException)
         true
         (do
           (t/log! :error {:id ::executor-error :data (str e#)})
           (t/error! {:level :debug :id ::executor-error}  e#)
           false)))))

(defn go!
  ([^Executor exec]
   (when (check-jump
           (if-let [task @(:next exec)]
             (do
               (t/event! ::jump-task {:data task :level :trace})
               (reset! (:next exec) nil)
               (run! task)))
           (if-let [task (.pollLast (:queue exec))]
             (do
               (t/event! ::pop-task {:data task :level :trace})
               (run! task))
             (if-let [work (steal! *coordinator* exec)]
               (push! exec work)
               (reset! (:running? exec) false))))
     (recur exec))))

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
