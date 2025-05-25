(ns janus.runtime
  (:refer-clojure :exclude [run!])
  (:require
   [janus.ast :as ast]
   [janus.env :as env]
   [janus.debug :refer [trace!]])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque)))

(def stack (ConcurrentLinkedDeque.))

;; A task cannot be scheduled until it is ready to run.
;;
;; This is a black box: we throw the task over the fence and are assured that it
;; will eventually run, but we have no indication of when.
(defn schedule [task]
  (.add stack task))

(defn run-task [[f arg]]
  (f arg))

(defn next-task []
  (try
    (.pop stack)
    (catch java.util.NoSuchElementException e
      (println "---"))))

(defn run! []
  (when-let [task (next-task)]
    (run-task task)
    (recur)))



(defn with-return [ccs cont]
  (assoc ccs (ast/xkeys :return) cont))

(defn send! [ccs chn msg]
  (let [err     (fn [_] (throw (RuntimeException. (str "No such channel: " chn))))
        unbound (get ccs (ast/xkeys :unbound) err)]
    (schedule [(get ccs chn unbound) msg])))

(defn perform-emit! [x ccs]
  (loop [kvs (env/kvs x)]
    (when (seq kvs)
      (let [[chn msg] (first kvs)]
        (trace! "sending on" chn ":" msg)
        (send! ccs chn msg))
      (recur (rest kvs)))))

(defn send-return! [v ccs]
  (send! ccs (ast/xkeys :return) v))

(def connection-rules
  {:E perform-emit!})

(defn connection [x]
  (get connection-rules (ast/type x) send-return!))

(defn connect [form ccs]
  ((connection form) form ccs))
