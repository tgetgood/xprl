(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.builtins :as builtins]
            [xprl.env :as env]
            [xprl.interpreter :as i]))

(defn enqueue! [exec task]
  (assert (ast/emission? task))
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
  (let [e (i/walk cable form)]
    (enqueue! exec (if (ast/emission? e)
                     e
                     (ast/emission cable [[(ast/xkey :return) e]])))))

(defn start! [exec]
  (let [ems (:work @exec)]
    ;; (println ems)
    (when (seq ems)
      ;; TODO: dosync for work stealing.
      (let [e (peek ems)]
        ;; (println (:env e))
        ;; Remove task from work stack *before* running it!
        (swap! exec update :work pop)
        (do-emission! exec e))
      (recur exec))))
