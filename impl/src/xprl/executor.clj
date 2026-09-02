;; REVIEW: Should this even be its own ns at this point?
(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.emission :as emit]))

(defn create! []
  (atom {:work  []
         :index {}}))

(defonce ^:dynamic *the-executor*Think IThink I nil)
(defonce root-task (gensym "root-task-"))
(defonce ^:dynamic *current-task* root-task)

(defonce tasks (atom {root-task {:id root-task :parent ::none :children #{}}}))

;; TODO: If none of the ancestors of `task` have completion handlers, then
;; there's no point indexing a task
(defn index-task! [{:keys [parent id] :as task}]
  (swap! tasks #(-> %
                    (update-in [parent :children] (fnil conj #{}) id)
                    (assoc id (assoc task :children #{})))))

(defn task {:style/indent [1]}
  ([env f] (task env f nil))
  ;; FIXME: I'm only allowing completion handlers to be created when a task is
  ;; created. It's trivial to just wrap tasks to add completion handlers, so I'm
  ;; not losing generality, but this is the kind of language design shortcut
  ;; that makes a language clunky.
  ;; So: Is this necessary?
  ([env f on-complete]
   (let [m (merge
            {:parent *current-task*
             :id     (gensym "task-")
             :env    env}
            (when on-complete
              ;; N.B.: This indexes the completion handler before the the task it
              ;; waits on is even created, but that's fine because it won't be
              ;; ~enqueued~ until after the waited-upon task is finished (which
              ;; had better be *after* it has been created).
              {:on-complete (task env on-complete)}))]
     (index-task! m)
     (with-meta f (merge (meta f) m)))))

(defn enqueue!
  ([task]
   (enqueue! *the-executor* task))
  ([exec task]
   (swap! exec update :work conj task)
   nil))

(defn enqueue-all!
  ([tasks] (enqueue-all! *the-executor* tasks))
  ([exec tasks] (swap! exec update :work #(into % tasks))))

(defn clear-finished [index id]
  (let [{:keys [parent children on-complete]} (get index id)]
    (assert (not (nil? parent)) (str id "has no parent:" (get index id)))
    (cond
      (= id root-task)  index
      (empty? children) (let [i' (-> index
                                     (dissoc id)
                                     (update-in [parent :children] disj id)
                                     (clear-finished parent))]
                          (with-meta i' (update (meta i') :cbs conj on-complete)))
      true              index)))

(defn deindex-task! [task]
  (let [index  @tasks
        index' (clear-finished index (:id (meta task)))]
    (if (compare-and-set! tasks index (with-meta index' {}))
      (when-let [completions (remove nil? (:cbs (meta index')))]
        (when (seq completions)
          ;;This has to be here since we DO NOT want to enqueue completion
          ;;callbacks more than once.
          (enqueue-all! completions)))
      ;; spin!
      (recur task))))

(defn run-task [t]
  (binding [*current-task* (:id (meta t))]
    (cond
      (fn? t) (let [{:keys [env]} (meta t)]
                   (t (emit/with-return env
                           (fn [x]
                             ;; REVIEW: Special baked-in behaviour of nets.
                             (if (ast/net? x)
                               (enqueue-all!
                                (map (fn [f]
                                       (task env #((:walkfn x) % (ast/immediate f))))
                                     (:forms x)))
                               (emit/return env x))))))
      true       (throw (RuntimeException. (str "Bad task type " (type t) ": " t))))
    (deindex-task! t)))

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
