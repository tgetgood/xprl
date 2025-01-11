(ns janus.runtime
  (:refer-clojure :exclude [run! delay])
  ;; FIXME: Minimise runtime requirements
  (:require [clojure.repl :as repl]
            [janus.ast :as ast]
            [taoensso.telemere :as t])
  (:import [java.util.concurrent ConcurrentLinkedDeque]))

(defn event!
  ([id] (event! id nil nil))
  ([id data] (event! id data nil))
  ([id data msg]
   (t/event! id {:level :trace :data data :msg msg})))

;;;;; Standard keys

;; TODO: Qualify these keys.
(def return (ast/keyword "return"))
(def error (ast/keyword "error"))
(def env (ast/keyword "env"))
(def delay (ast/keyword "delay"))

(def unbound
  (with-meta (ast/keyword "xprl.core.channels.unbound")
    {:name "Unbound channel handler"}))

;;;;; Tasks

(defn steal! [system exec]
  ;; REVIEW: What to log here? Executors ought to have names.
  (event! ::work-steal)
  ;; TODO: steal work!
  (t/log! :debug "Work queue empty. Nothing to do")
  nil)

(defprotocol ITask
  (run! [this]))

(defrecord Task [f arg id meta]
    ITask
    (run! [_]
      (event! ::run.task {:fn f :meta meta :arg arg :id id})
      (f arg)))

(defn task
  ([f arg]
   (task f arg {}))
  ([f arg m]
   (let [id (gensym)]
     (->Task f arg id m))))


(defrecord BarrierTask [f meta]
  ITask
  (run! [_]
    (event! ::run.barrier meta)
    (f)))

(declare ^:dynamic *executor*)

;;;;; Executor

(defprotocol Queue
  (push! [this tasks])
  (insert! [this task] "Insert a single task but don't jump.")
  (go! [this]))

(defprotocol Initable
  (start! [this])
  (stop! [this])
  (running? [this]))

(defprotocol FnCache
  (clear! [this])
  (store! [this task])
  (next-task [this]))

(defmacro check-jump
  {:style/indent 0}
  [& body]
  `(try
     ~@body
     true
     (catch RuntimeException e#
       ;; REVIEW: Is halting the correct thing to do here? Should we not make
       ;; our best effort to keep going?
       (t/log! :error {:id ::executor-error :data (str e#)})
       (t/error! {:level :debug :id ::executor-error}  e#)
       (stop! *executor*)
       (alter-var-root #'clojure.core/*e (fn [_#] e#))
       false)))

(deftype Executor [^ConcurrentLinkedDeque queue
                   ^:unsynchronized-mutable next
                   ^:unsynchronized-mutable running?]

  Queue
  (push! [this tasks]
    (when next
      (insert! this next))

    (set! next (first tasks))

    (if (= 2 (count tasks))
      (.add queue (second tasks))
      (.addAll queue (rest tasks)))
    (when-not running?
      (start! this)))
  (insert! [this task]
    (.add queue task))
  (go! [this]
    (if-let [task (next-task this)]
      (when (check-jump (run! task))
        (recur))
      (if-let [work (steal! nil this)]
        (push! this work)
        (stop! this))))

  Initable
  (start! [this]
    (set! running? true)
    (go! this))
  (stop! [_]
    (set! running? false))
  (running? [_]
    running?)

  FnCache
  (clear! [_]
    (set! next nil))
  (store! [_ task]
    (set! next task))
  (next-task [_]
    (if next
      (let [n' next]
        (set! next nil)
        n')
      (.pollLast queue))))

(defn create-executor []
  ;; REVIEW: ConcurrentLinkedDeque is obviously too much overhead for the main
  ;; event loop of a real system. The design calls for singlethreaded dequeues
  ;; with a steal operation that needs to be somewhat coordinated. We'll get
  ;; there eventually.
  (->Executor (ConcurrentLinkedDeque.) nil false))

(def ^:dynamic *coordinator* nil)
(def ^:dynamic *executor* (create-executor))

(defn insert-barrier! [f meta]
  (insert! *executor* (->BarrierTask f meta)))

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
  (fn [args]
    (t/log! {:level :warn :data {:name c :args args}}
            "Message sent on unbound channel")))

(defn parse-emission [c [k v]]
  (event! ::parse-raw [k (fn? k) (ast/keyword? k) (contains? c k)])
  (cond
    ;; TODO: What kind of function?
    (fn? k) (do
              (event! ::emit.fn {:fn k :args v})
              (task k v (merge (meta k) {:ch :none})))

    (ast/keyword? k)
    ;; Nested conds. real nice.
    (cond
      (contains? c k)
      (do
        (event! ::emit.bound {:ch-name k :ch (get c k) :msg v})
        (task (get c k) v (merge (dissoc (meta k) :lex) {:ch-name k})))

      (contains? c unbound)
      (do
        (event! ::emit.unbound.caught {:ch-name k :msg v})
        (task (get c unbound) {:ch-name k :msg v}))

      ;; It isn't an error to send a message to nobody, even though that might
      ;; break the system if somebody needs that message.
      ;; REVIEW: What to do about that?
      :else (event! ::emit.unbound.uncaught {:ch-name k :msg v} "Unresolved channel"))

    :else (do
            (t/log! {:level :error
                     :data  [(type k) k]}
                    "Only fns and xprl keywords can be channels.")
            (throw (RuntimeException.
                    (str k " is not a valid channel."))))))

(defn emit
  "For each `kv`, sends `v` to continuation named by `k` in the continuation map
  `c` if `k` is a keyword. If `k` is a function it is assumed to *be* the
  continuation."
  {:style/indent 1}
  [c & kvs]
  (event! ::emit-raw kvs)
  (let [tasks (map (partial parse-emission c) (partition 2 kvs))]
    (event! ::emit-tasks tasks)
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

(defn unbound-collector
  "Returns a channel which collects all events sent to it, calling `next` with
  the aggregate at some point after the channel can no longer receive input
  (because all tasks which have access to it have completed)."
  [next]
  (let [name      (gensym "collector")
        collector (atom [])
        receive   (fn [msg]
                    (event! ::collector.receive {:name name} msg)
                    (swap! collector conj msg))
        done      (fn []
                    (let [delays @collector]
                      (event! ::collector.done {:name name :coll delays})
                      (next delays)))]
    (event! ::collector.create {:name name})
    (insert-barrier! done name)
    receive))

(defn receive
  "Send the value `v` to the `i`th slot of collector c."
  [c i v]
  (event! ::v.collector.receive [i v])
  (swap! c
   (fn [{:keys [n unset elements] :as c}]
     (when-not (and (> n 0) (= (get elements i) unset))
       (t/error! (throw (RuntimeException.))))
     (-> c
         (update :n dec)
         (update :elements assoc i v))))
  (when (= 0 (:n @c))
    (let [{:keys [next elements unset]} @c]
      (event! ::v.collector.done elements)
      (emit next return elements))))
