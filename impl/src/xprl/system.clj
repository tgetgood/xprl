(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.executor :as exec]
            [xprl.interpreter :as i]))

(defonce the-system (atom nil))

(defn init! []
  (compare-and-set! the-system nil {:executors [(exec/create!)]}))

;; The clj repl interacts with the executor threads by injecting work into the
;; queue of one of theme. Which? it doesn't matter, in principle. Just use the
;; first for now.
(defn seed! [task]
  (exec/enqueue! (first (:executors @the-system)) task)
  nil)

(defn start-executors! []
  ;; Each executor owns a thread.
  (run! #(.start (Thread. (fn run [] (exec/run %)))) (:executors @the-system)))

(defn start! []
  (init!)
  (start-executors!))
