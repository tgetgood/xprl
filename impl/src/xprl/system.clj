(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.executor :as exec]))

(defonce the-system (atom nil))

(defn init! []
  (compare-and-set! the-system nil {:executors [(exec/create!)]}))

(defn seed! [env form]
  (exec/seed! (first (:executors @the-system)) env form))

(defn start-executors! []
  (run! exec/start! (:executors @the-system)))

(defn start! [cable form]
  (init!)
  (seed! cable form)
  (start-executors!))
