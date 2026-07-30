;; REVIEW: Should this even be its own ns at this point?
(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.continuation :as cont]))

(def ^:dynamic *the-executor* nil)

(defn enqueue!
  ([task]
   (enqueue! *the-executor* task))
  ([exec task]
   (swap! exec update :work conj task)))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec cable form]
  (enqueue! exec [cable form]))

(defn start! [exec walk]
  (when-not (:running? @exec)
    (binding [*the-executor* exec]
      (loop []
        (let [ems (:work @exec)]
          ;; (println ems)
          (if (seq ems)
            ;; TODO: dosync for work stealing.
            (let [[env form] (peek ems)]
              ;; Remove task from work stack *before* running it!
              (swap! exec update :work pop)
              ;; This should block the thread until it goes to sleep
              (walk env form)
              ;; At which point we find something else to do
              (recur))
            (swap! exec assoc :running? false)))))))
