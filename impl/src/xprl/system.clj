(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.executor :as exec]
            [xprl.interpreter :as i]))

(defonce the-system (atom nil))

(defn init! []
  (compare-and-set! the-system nil {:executors [(exec/create!)]}))

;; The clj repl interacts with the executor threads by injecting work into the
;; queue of one of theme. Whick? it doesn't matter, in principle. Just use the
;; first for now.
(defn seed! [tasks]
  ;; Atomically add a set of tasks to make sure none start until all have been
  ;; enqueued.
  ;; REVIEW: turns out I don't actually have a use for this, so should I keep
  ;; it? Nice to know the option is there, and it isn't really any more
  ;; overengineered that the old version.
  (exec/enqueue-all! (first (:executors @the-system)) tasks)
  nil)

(defn start-executors! []
  ;; Each executor owns a thread.
  (run! #(.start (Thread. (fn run [] (exec/run %)))) (:executors @the-system)))

(defn start! []
  (init!)
  (start-executors!))
