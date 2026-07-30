(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.continuation :as cont]))

(def ^:dynamic *the-executor* nil)

(defn enqueue!
  ([task] (enqueue! *the-executor* task))
  ([exec task] (swap! exec update :work conj task)))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec cable form]
  (enqueue! exec [cable form]))

(defn process-walked [exec env form]
  (when form
    (if (ast/net? form)
      (let [env (merge env (:env form))]
        (run! #(enqueue! exec [env (ast/immediate %)]) (:forms form)))
      (do
        ;; (println "stiching return value: " form)
        (assert (contains? env cont/ret) (str "Cannot return " form ". No destination."))
        ((get env cont/ret) form)))))

(defn start! [exec walk]
  (when-not (:running? @exec)
    (binding [*the-executor* exec]
      (loop []
        (let [ems (:work @exec)]
          (if (seq ems)
            ;; TODO: dosync for work stealing.
            (let [[env form] (peek ems)]
              ;; Remove task from work stack *before* running it!
              (swap! exec update :work pop)
              ;; This should block the thread until it goes to sleep
              (cont/ret-> env #(walk % form) (partial process-walked exec env))
              ;; At which point we find something else to do
              (recur))
            (swap! exec assoc :running? false)))))))
