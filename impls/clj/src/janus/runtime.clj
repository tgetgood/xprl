(ns janus.runtime
  (:refer-clojure :exclude [run! delay])
  (:require
   [janus.ast :as ast]
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
;; Tasks are just functions of no args with possible metadata.
;; REVIEW: Why not just use functions?
;; Because of barrier tasks. But do we really need those?

(defprotocol ITask
  (run! [this]))

(defrecord Task [f meta]
    ITask
    (run! [_]
      (event! ::run.task {:fn f :meta meta})
      (f)))

(defn task
  ([f]
   (task f {}))
  ([f m]
   (->Task f m)))

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

(defn withcc
  {:style/indent 1}
  ([c m]
   (merge c (sanitise-keys m)))
  ([c k v & kvs]
   (withcc c (apply hash-map k v kvs))))

;;;;; Channels
;;
;; Channels are part of the runtime, but not part of the language itself. I'm
;; conflicted about making first class access to channels a part of the
;; language. Manual putting is certainly a bad idea, but the ability to "reify"
;; a channel as a lazy seq that parks when trying to read past the end could be
;; useful.
;;
;; These are going to be a huge amount of overhead for returning a value. I need
;; a datastructure that can use a single storage cell in the overwhelming
;; majority of cases and only upgrade to a queue when necessary.
;;
;;;;;

(defprotocol IChannel
  (close! [ch] "close the channel")
  (on-close! [ch cb] "react to a closing channel")
  (<! [ch cont] "take a value from a channel")
  (>! [ch val cont] "put a value on the channel"))

;; Channels can have multiple writers, but only a single reader. This channel
;; type assumes that that single reader parks if it can't take and never tries
;; to take multiple values concurrently with itself.
;;
;; I don't think it's possible for a single process to try and take while
;; parked. The assertion here is mostly to prevent multiple readers, though it
;; won't catch them in general. I need to statically check network
;; constructions.
(defrecord SingleReaderChannel [^ConcurrentLinkedDeque puts take close]
  IChannel
  (>! [_ val cont]
    (if-let [take-cont @take]
      (if (compare-and-set! take take-cont nil)
        (push! [(task (fn [] (take-cont val))) (task cont)])
        (assert false "multiple read error."))
      (.add puts {:val val :cont cont})))

  (<! [_ take-cont]
    (let [stored @take]
      (if-let [{:keys [val cont]} (.poll puts)]
        (push! [(task (fn [] (take-cont val))) (task cont)])
        (if-not (nil? stored)
          (assert false "multiple reader error")
          (if (compare-and-set! take nil take-cont)
            nil
            (assert false "concurrent read error.")))))))

(defn chan []
  (->SingleReaderChannel (ConcurrentLinkedDeque.)
                         (atom nil)
                         (atom {:closed? false :cbs []})))
