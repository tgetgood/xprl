(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.executor :as exec]
            [xprl.interpreter :as i]))

(defonce the-system (atom nil))

(defn init! []
  (compare-and-set! the-system nil {:executors [(exec/create!)]}))

(defn seed! [env form]
  (exec/seed! (first (:executors @the-system)) env form))

(defn start-executors! []
  ;; TODO: Start these in threads!

  ;; We pass the interpreter to the executor to break a dependency cycle.
  ;; This seems a little off...
  (run! #(exec/start! % i/walk) (:executors @the-system)))

(defn start! [cable form]
  (init!)
  (seed! cable form)
  (start-executors!))
