;; REVIEW: Should this even be its own ns at this point?
(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.continuation :as cont]))

(defn create! []
  (atom {:work  []
         :index {}}))

(defonce ^:dynamic *the-executor* nil)

(defn enqueue!
  ([task]
   (enqueue! *the-executor* task))
  ([exec task]
   (swap! exec update :work conj task)
   nil))

(defn enqueue-all!
  ([tasks] (enqueue-all! *the-executor* tasks))
  ([exec tasks] (swap! exec update :work #(into % tasks))))

(defn run-task [task]
  ;; (when (meta task) (println "run" (meta task)))
  (cond
    (fn? task) (task)
    true       (throw (RuntimeException. (str "Bad task type " (type task) ": " task)))))

;; The executor "queue" is actually a stack, so this task acts as a barrier and
;; will be executed exactly once when all work deriving from the current task is
;; finished, but before moving on the the next task.
;;
;; At least that's the theory.
;;
;; It fails both in the face of parking and work stealing, so I need a better
;; theory.
(defn on-complete! [cb]
  (enqueue! *the-executor* cb))

(defn run [exec]
  (binding [*the-executor* exec]
    (loop []
      (try
        (let [ems (:work @exec)]
          (if (seq ems)
            ;; TODO: dosync for work stealing.
            (let [task (peek ems)]
              ;; Remove task from work stack *before* running it!
              (swap! exec update :work pop)
              ;; This should block the thread until it goes to sleep
              (run-task task))
            (Thread/sleep 500)))
        (catch Throwable e
          (binding [*out* *err*]
            (println e))))
      ;; At which point we find something else to do
      (recur))))
