(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.builtins :as builtins]
            [xprl.env :as env]
            [xprl.interpreter :as i]))

(defn enqueue! [exec task]
  (assert (fn? task))
  (swap! exec update :work conj task))

(defn enqueue-task! [exec f args]
  (enqueue! exec (f args))
  nil)

;; REVIEW: `env` now refers to the cable. This is confusing.
(defn send! [exec env k v]
  (if (contains? env k)
    (enqueue-task! exec (get env k) v)
    (do (println env)
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn enqueue-emission! [exec {:keys [env msgs]}]
  (run! (fn [[k v]] (send! exec env k v)) msgs))

(defn run-task! [exec task]
  (let [v (task)]
    (cond (ast/emission? v) (enqueue-emission! exec v)
          (nil? v)          nil
          true              (println "WARNING: dropping non-emission return value." v))))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec cable form]
  (enqueue! exec #(i/walk cable form)))

(defn start! [exec]
  (let [tasks (:work @exec)]
    (when (seq tasks)
      ;; TODO: dosync for work stealing.
      (let [t (peek tasks)]
        ;; Remove task from work stack *before* running it!
        (swap! exec update :work pop)
        (run-task! exec t))
      (recur exec))))
