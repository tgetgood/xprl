(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.env :as env]
            [xprl.emission :as emit]
            [xprl.interpreter :as i]))

(defn enqueue! [exec task]
  (swap! exec update :work conj task))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec cable form]
  (enqueue! exec [cable form]))

(defn process-walked [exec env form]
  (when form
    (if (ast/net? form)
      (run! #(enqueue! exec [(:env form) (ast/immediate %)]) (:forms form))
      (do
        ;; (println "stiching return value: " form)
        (assert (contains? env emit/ret) (str "Cannot return " form ". No destination."))
        ((get env emit/ret) form)))))

(defn start! [exec]
  (when-not (:running? @exec)
    (let [ems (:work @exec)]
      (if (seq ems)
        ;; TODO: dosync for work stealing.
        (let [[env form] (peek ems)]
          ;; Remove task from work stack *before* running it!
          (swap! exec update :work pop)
          ;; This should block the thread until it goes to sleep
          (emit/ret-> env #(i/walk % form) (partial process-walked exec env))
          ;; At which point we find something else to do
          (recur exec))
        (swap! exec assoc :running? false)))))
