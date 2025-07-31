(ns janus.runtime
  (:refer-clojure :exclude [run!])
  (:require
   [janus.ast :as ast]
   [janus.debug :refer [trace!]]
   [janus.env :as env]
   [janus.interpreter :as i])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque)))

(def stack (ConcurrentLinkedDeque.))

;; A task cannot be scheduled until it is ready to run.
;;
;; This is a black box: we throw the task over the fence and are assured that it
;; will eventually run, but we have no indication of when.
(defn schedule [task]
  (.add stack task))

(defn run-task [t]
  (assert (ast/list? t))
  (let [[f arg] t]
    (f arg)))

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
        unbound #((get ccs (ast/xkeys :unbound) err) [chn %])]
    (schedule (ast/list [(get ccs chn unbound) msg]))))

(defn perform-emit! [x ccs]
  (loop [kvs (i/walk* (:kvs x))]
    (when (seq kvs)
      (let [[chn msg] (first kvs)]
        (trace! "sending on" chn ":" msg)
        (send! ccs chn msg))
      (recur (rest kvs)))))

(defn pass-ccs [ν ccs]
  (trace! "\ninvoke ν\n" (:body ν) "\n")
  ;; REVIEW: Invocation of a ν *cannot* extend the context...
  (binding [i/*env* {(:params ν) (env/pin ccs i/*env*)}]
    (update ν :body i/walk*)))

(defn send-return! [v ccs]
  (send! ccs (ast/xkeys :return) v))

(declare connect)

;; REVIEW: seqs and concs can't actually bubble up to connect can they? I think
;; this is a dead end.
(defn schedule-concurrent! [{xs :elements} ccs]
  ;; Assume every element is connectable and let the runtime sort it out.
  ;; The only dependencies between concurrent tasks are data dependencies
  ;; managed by channels/streams
  (dorun (map (fn [x] (schedule (ast/list [(fn [_] (connect x ccs))]))) xs)))

(defn run-sequential! [{xs :elements} ccs]
  ;; FIXME: This will not work for concs nested within seqs since we need some
  ;; sort of barrier to trigger when all elements of a conc are finished upon
  ;; which we can wait.
  ;;
  ;; REVIEW: Is it well defined to await a conc being "finished"?
  (loop [[x & more] xs]
    (connect x ccs)
    (when (seq more)
      (recur more))))

(def connection-rules
  {:E perform-emit!
   :ν pass-ccs

   :seq  run-sequential!
   :conc schedule-concurrent!})

(defn connection [x]
  (get connection-rules (ast/type x) send-return!))

(defn connect [form ccs]
  (if (env/ctx? form)
    (binding [i/*env* (:env form)]
      (connect (:form form) ccs))
    (do
      ((connection form) form ccs)
      :end-of-computation)))
