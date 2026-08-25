;; REVIEW: Should this even be its own ns at this point?
(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.continuation :as cont]))

(defn create! []
  (atom {:work  []
         :index {}}))

(defonce ^:dynamic *the-executor* nil)
(defonce root-task (gensym "root-task-"))
(defonce ^:dynamic *current-task* root-task)

(defonce tasks (atom {root-task {:id root-task :parent ::none :children #{}}}))

(defn task
  ([f] (task f nil))
  ;; FIXME: I'm only allowing completion handlers to be created when a task is
  ;; created. It's trivial to just wrap tasks to add completion handlers, so I'm
  ;; not losing generality, but this is the kind of language design shortcut
  ;; that makes a language clunky.
  ;; So: Is this necessary?
  ([f on-complete]
   (with-meta f
     (merge {:parent *current-task* :id (gensym "task-")}
            (when on-complete {:on-complete (task on-complete)})))))

;; TODO: If none of the ancestors of `task` have completion handlers, then
;; there's no point indexing a task
(defn index-task! [task]
  (let [{:keys [parent id]} (meta task)]
    (println "indexing" id "<-" parent)
    (swap! tasks #(-> %
                      (update-in [parent :children] (fnil conj #{}) id)
                      (assoc id (assoc (meta task) :children #{}))))
    task))

(defn enqueue!
  ([task]
   (enqueue! *the-executor* task))
  ([exec task]
   (swap! exec update :work conj (index-task! task))
   nil))

(defn enqueue-all!
  ([tasks] (enqueue-all! *the-executor* tasks))
  ([exec tasks] (swap! exec update :work #(into % (map index-task!) tasks))))

(defn clear-finished [index id]
  (let [{:keys [parent children on-complete]} (get index id)]
    (cond
      (= id root-task)  index
      (empty? children) (do
                          (println "deindexing" id)
                          (let [i' (clear-finished (dissoc index id) parent)]
                            (with-meta i' (update (meta i') :cbs conj on-complete))))
      true              index)))

(defn deindex-task! [task]
  (println "completing" (:id (meta task)) "children:"
           (get-in @tasks [(:id (meta task)) :children]))
  (let [index  @tasks
        index' (clear-finished index (:id (meta task)))]
    (if (compare-and-set! tasks index (with-meta index' {}))
      (when-let [completions (remove nil? (:cbs (meta index')))]
        (when (seq completions)
          ;;This has to be here since we DO NOT want to enqueue completion
          ;;callbacks more than once.
          (println "enqueue completions" (map meta completions))
          (enqueue-all! completions)))
      ;; spin!
      (do (println "spin") (recur task)))))

(defn run-task [task]
  ;; (when (meta task) (println "run" (meta task)))
  (binding [*current-task* (:id (meta task))]
    (println "running task" (:id (meta task)))
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
