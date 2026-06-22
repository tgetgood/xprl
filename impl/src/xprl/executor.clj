(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.env :as env]
            [xprl.interpreter :as i]))

(defn enqueue! [exec task]
  (swap! exec update :work conj task))

;; REVIEW: `env` now refers to the cable. This is confusing.
(defn send! [exec env k v]
  (if (contains? env k)
    (let [res ((get env k) v)]
      (when res
        (if (ast/emission? res)
          (enqueue! exec res)
          (println "WARNING: dropping non-emission result:" res))))
    (do (println env)
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn do-emission! [exec {:keys [env msgs]}]
  (run! (fn [[k v]] (send! exec env k v)) msgs))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec cable form]
  (enqueue! exec [cable form]))

(defn start! [exec]
  (when-not (:running? @exec)
    (let [ems (:work @exec)]
      (if (seq ems)
        ;; TODO: dosync for work stealing.
        (let [[env form] (peek ems)]
          ;; Remove task from work stack *before* running it!
          (swap! exec update :work pop)
          (let [res (i/walk env form)]
            (do-emission! exec
                          (if (ast/emission? res)
                            res
                            (ast/emission env [[(ast/xkey :return) res]]))))
          (recur exec))
        (swap! exec assoc :running? false)))))
