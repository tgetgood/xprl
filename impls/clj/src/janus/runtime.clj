(ns janus.runtime
  (:refer-clojure :exclude [run! delay])
  (:require
   [clojure.repl :as repl]
   [janus.ast :as ast]
   [janus.interpreter :as i]
   [taoensso.telemere :as t])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque)))

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

(defprotocol ITask
  (run! [this]))

(defrecord Task [f arg meta]
    ITask
    (run! [_]
      (event! ::run.task {:fn f :meta meta :arg arg})
      (f arg)))

(defn task
  ([f arg]
   (task f arg {}))
  ([f arg m]
   (->Task f arg m)))

(defrecord BarrierTask [f meta]
  ITask
  (run! [_]
    (event! ::run.barrier meta)
    (f)))

;;;;; Executor

(defrecord Executor [^ConcurrentLinkedDeque queue running?])

(defn create-executor []
  (->Executor (ConcurrentLinkedDeque.) (atom false)))

(def ^:dynamic *coordinator* nil)
(def ^:dynamic *executor* (create-executor))

(defn stop! []
  (event! ::executor.stop)
  (reset! (:running? *executor*) false))

(defmacro check-run
  "Catches and logs any execeptions that escape from running body. Stops
  executor if anything is caught."
  [& body]
  `(try
     ~@body
     (catch RuntimeException e#
       ;; REVIEW: Is halting the correct thing to do here? Should we not make
       ;; our best effort to keep going?
       (stop!)
       (t/error! {:id ::executor-error}  e#)
       (alter-var-root #'clojure.core/*e (fn [_#] e#)))))

(defn go! []
  (let [{:keys [queue running?]} *executor*]
    (when @running?
      (if-let [next (.pollLast ^ConcurrentLinkedDeque queue)]
        (do
          (event! ::run next)
          (check-run (run! next))
          (recur))
        (do
          ;; TODO: Steal work!
          (stop!))))))

(defn resume! []
  (when-not @(:running? *executor*)
    (event! ::executor.resume)
    (reset! (:running? *executor*) true)
    (go!)))

(defprotocol Exec
  (push* [this queue]))

(extend-protocol Exec
  janus.runtime.Task
  (push* [this queue]
    (.add ^ConcurrentLinkedDeque queue this))

  janus.runtime.BarrierTask
  (push* [this queue]
    (.add ^ConcurrentLinkedDeque queue this))

  clojure.lang.IPersistentCollection
  (push* [this queue]
    (.addAll ^ConcurrentLinkedDeque queue this)))

(defn push!
  "Add tasks to work stack of the current executor.
  Does not start processing."
  [tasks]
  (push* tasks (:queue *executor*))
  (resume!))

(defn insert-barrier! [f meta]
  (push! (->BarrierTask f meta)))

;;;;; Collectors

(defn double-set-error! [_ _ _]
  (throw (RuntimeException. "!!!")))

(defn unordered-collector
  "Returns a channel which collects all events sent to it, calling `next` with
  the aggregate at some point after the channel can no longer receive input
  (because all tasks which have access to it have completed)."
  [init next]
  (let [name      (gensym "unordered-collector")
        collector (atom init)
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

(defn ordered-collector
  "Returns a collector which will collect `n` things and then call `next` with
  the result."
  {:style/indent 1}
  [n next]
  (let [name      (gensym "ordered-collector")
        collector (atom {:n n :elements (into [] (take n) (repeat name))})]
    (event! ::o.collector.create {:name name :size n})
    (fn [i v]
      (swap! collector
             (fn [{:keys [elements n] :as c}]
               (event! ::o.collector.receive {:name name :i i :v v :remaining n})
               (when-not (and (> n 0) (= (get elements i) name))
                 (double-set-error! elements i v))
               (-> c
                   (update :n dec)
                   (update :elements assoc i v))))
      (let [{:keys [n elements]} @collector]
        (when (= 0 n)
          (event! ::o.collector.done {:name name :coll elements})
          (next elements))))))

;;;;; Emission

(defn sanitise [k]
  (assert (ast/keyword? k) "Only xprl keywords can be used in cc maps.")
  k)

(defn sanitise-keys [m]
  (into (empty m) (map (fn [[k v]] [(sanitise k) v])) m))

;; FIXME: This will be a lot less ugly and more extensible if reworked with a protocol.
(defn parse-emission [c [k v]]
  (event! ::parse-raw [k (fn? k) (ast/keyword? k) (contains? c k)])
  (cond
    ;; TODO: What kind of function?
    (fn? k) (do
              (event! ::emit.fn {:fn k :args v})
              (task k v (merge (meta k) {:ch ::fn})))

    (instance? janus.ast.Mu k)
    (do
      (event! :emit.μ {:μ k :args v})
      (task #(i/apply k % {} c) v (merge (meta k) {:ch ::μ})))

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
  [c kvs]
  (event! ::emit-raw kvs)
  (let [tasks (map (partial parse-emission c) kvs)]
    (event! ::emit-tasks tasks)
    (push! tasks)))

(defn withcc
  {:style/indent 1}
  ([c m]
   (merge c (sanitise-keys m)))
  ([c k v & kvs]
   (withcc c (apply hash-map k v kvs))))
