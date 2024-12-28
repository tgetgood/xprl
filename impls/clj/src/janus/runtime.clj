(ns janus.runtime
  (:refer-clojure :exclude [run! delay])
  ;; FIXME: Minimise runtime requirements
  (:require [clojure.repl :as repl]
            [janus.ast :as ast]
            [taoensso.telemere :as t])
  (:import [java.util.concurrent ConcurrentLinkedDeque]))

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
  (t/event! ::work-steal {:level :trace})
  ;; TODO: steal work!
  (t/log! :debug "Work queue empty. Nothing to do")
  nil)

(defprotocol ITask
  (run! [this]))

(defrecord Task [f arg meta]
    ITask
    (run! [_]
      (t/event! ::run {:level :trace :data {:fn f :meta meta :arg arg}})
      (f arg)))

(defn task
  ([f arg]
   (task f arg {}))
  ([f arg m]
   (->Task f arg m)))

(declare ^:dynamic *executor*)

;;;;; Executor

(defprotocol Queue
  (push! [this tasks])
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
    (if next
      (if (= 1 (count tasks))
        (.add queue (first tasks))
        (.addAll queue tasks))
      (do
        (set! next (first tasks))
        (if (= 2 (count tasks))
          (.add queue (second tasks))
          (.addAll queue (rest tasks)))))
    (when-not running?
      (start! this)))
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
  (cond
    ;; TODO: What kind of function?
    (fn? k) (do
              (t/event! ::emit.fn {:level :trace :data {:fn k :args v}})
              (task k v (merge (meta k) {:ch :none})))

    (ast/keyword? k)
    ;; Nested conds. real nice.
    (cond
      (contains? c k)
      (do
        (t/event! ::emit.bound {:level :trace
                                :data  {:ch-name k :ch (get c k) :msg v}})
        (task (get c k) v (merge (dissoc (meta k) :lex) {:ch-name k})))

      (contains? c unbound)
      (do
        (t/event! ::emit.unbound.caught {:level :trace
                                         :data  {:ch-name k :msg v}})
        (task (get c unbound) {:ch-name k :msg v}))

      ;; It isn't an error to send a message to nobody, even though that might
      ;; break the system if somebody needs that message.
      ;; REVIEW: What to do about that?
      :else (t/event! ::emit.unbound.uncaught
                      {:level :warn
                       :data  {:ch-name k :msg v}
                       :msg   "Unresolved channel"}))

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
  (let [tasks (map (partial parse-emission c) (partition 2 kvs))]
    (t/event! ::emit {:level  :trace :data {:raw kvs :tasks tasks}})

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
     (when-not (and (> n 0) (= (get elements i) unset))
       (t/error! (throw (RuntimeException.))))
     (-> c
         (update :n dec)
         (update :elements assoc i v))))
  (when (= 0 (:n @c))
    (let [{:keys [next elements unset]} @c]
      (t/event! :collector/join {:level :trace :data elements})
      (emit next return elements))))
