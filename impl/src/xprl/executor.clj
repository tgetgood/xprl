(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.env :as env]
            [xprl.interpreter :as i]))

(defn enqueue! [exec task]
  (swap! exec update :work conj task))

;; REVIEW: `env` now refers to the cable. This is confusing.
(defn send! [exec env [k v]]
  (if (contains? env k)
    ((get env k) v)
    (do (println env)
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn do-emission! [exec env msgs]
  (run! (partial send! exec env) msgs))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec cable form]
  (enqueue! exec [cable form]))

(defn process-walked [exec env form]
  (when form
    (if (ast/net? form)
      (run! #(enqueue! exec [(:env form) (ast/immediate %)]) (:forms form))
      (do-emission! exec env
                    (if (ast/emission? form)
                      (:msgs form)
                      [[(ast/xkey :return) form]])))))

(defn start! [exec]
  (when-not (:running? @exec)
    (let [ems (:work @exec)]
      (if (seq ems)
        ;; TODO: dosync for work stealing.
        (let [[env form] (peek ems)]
          ;; Remove task from work stack *before* running it!
          (swap! exec update :work pop)
          ;; This should block the thread until it goes to sleep
          (i/ret-> env #(i/walk % form) (partial process-walked exec env))
          ;; At which point we find something else to do
          (recur exec))
        (swap! exec assoc :running? false)))))
