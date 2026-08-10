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

(defn reseed! [walk env]
  (fn [x]
    (cond
      ;; We intentionally don't set the executor so that whomsoever stole this
      ;; task gets its follow up work.
      (ast/net? x)      (let [{:keys [env forms]} x]
                          (enqueue-all!
                           (mapv (fn [f] (fn [] (walk env (ast/immediate f))))
                                 forms)))
      (ast/emission? x) (throw (RuntimeException.
                                "Emission escaped as data! This is an error!"))
      true              (cont/return env x))))

;; Seeding is special because these are truly "top level" even if the term is
;; underdefined. So we can catch things like Nets and Emissions being sent up
;; out of the "current program", which means something different.
;;
;; The outer scope is a different interpreter, after all.
(defn seed! [exec walk tasks]
  (enqueue-all!
   exec
   (mapv (fn [[env form]]
           (fn [] (walk (cont/with-return env (reseed! walk env)) form)))
         tasks)))

(defn run-task [task]
  (cond
    (fn? task) (task)
    true       (throw (RuntimeException. (str "Bad task type " (type task) ": " task)))))

;; The executor "queue" is actually a stack, so this task acts as a barrier and
;; will be executed exactly once when all work deriving from the current task is
;; finished, but before moving on the the next task.
;;
;; At least that's the theory.
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
