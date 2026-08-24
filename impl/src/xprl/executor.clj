;; REVIEW: Should this even be its own ns at this point?
(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.continuation :as cont]))

(defn create! []
  (atom {:work  []
         :index {}}))

(defonce ^:dynamic *the-executor* nil)
(defonce ^:dynamic *current-task* (gensym "root-task-"))

(defonce tasks (atom {*current-task* #{}}))

(defmacro task
  ([body]
   `(with-meta (fn [] ~body)
      {:parent *current-task* :id (gensym "task-") :body '~body}))
  ;; FIXME: I'm only allowing completion handlers to be created when a task is
  ;; created. It's trivial to just wrap tasks to add completion handlers, so I'm
  ;; not losing generality, but this is the kind of language design shortcut
  ;; that makes a language clunky.
  ;; So: Is this necessary?
  ([body on-complete]
   (let [t `(task ~body)]
     `(with-meta ~t (merge (meta ~t) {:on-complete (task ~on-complete)})))))

;; TODO: If none of the ancestors of `task` have completion handlers, then
;; there's no point indexing a task
(defn index-task! [task]
  (let [{:keys [parent id]} (meta task)]
    (swap! tasks update parent (fnil conj #{}) id)
    task))

(defn enqueue!
  ([task]
   (enqueue! *the-executor* task))
  ([exec task]
   (swap! exec update :work conj (index-task! task))
   nil))

(defn deindex-task! [task]
  (let [{:keys [parent id on-complete body]} (meta task)

        index     @tasks
        children  (disj (get index parent) id)
        new-index (if (empty? children)
                    (dissoc index parent)
                    (assoc index parent children))]
    (if (compare-and-set! tasks index new-index)
      (when (empty? children)
        (when on-complete
          ;; (println "finished" body "starting" (:body (meta on-complete)) )
          (enqueue! on-complete)))
      ;; spin!
      (recur task))))

(defn enqueue-all!
  ([tasks] (enqueue-all! *the-executor* tasks))
  ([exec tasks] (swap! exec update :work #(into % (map index-task!) tasks))))

(defn run-task [task]
  ;; (when (meta task) (println "run" (meta task)))
  (binding [*current-task* (:id (meta task))]
    ;; (println "running task" (meta task))
    (cond
      (fn? task) (task)
      true       (throw (RuntimeException. (str "Bad task type " (type task) ": " task))))
    ;; (println "cleaning up" (meta task))
    (deindex-task! task)))

;; The executor "queue" is actually a stack, so this task acts as a barrier and
;; will be executed exactly once when all work deriving from the current task is
;; finished, but before moving on the the next task.
;;
;; At least that's the theory.
;;
;; It fails both in the face of parking and work stealing, so I need a better
;; theory.
#_(defn on-complete! [task]
  (enqueue! *the-executor* task))

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
