(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.builtins :as builtins]
            [xprl.env :as env]
            [xprl.interpreter :as i]))

(defn walk-task [form]
  (fn [env] (i/walk env form)))

(defn μ-task [μ args]
  (let [bindings {(:id μ)  args
                  (:rec μ) μ}]
    (walk-task (env/invoke bindings (:body μ)))))

(defn external-task [f args]
  (fn [env] (ast/call env f args)))

(defn task [f args]
  (cond
    (ast/μ? f)        (μ-task f args)
    (ast/external? f) (external-task f args)
    true              (throw (RuntimeException. (str "cannot enqueue " f)))))

(defn enqueue! [exec task]
  (swap! exec update :work conj task))

(defn enqueue-task! [exec f args]
  (enqueue! exec (task f args))
  nil)

(defn send! [exec env k v]
  (if (contains? env k)
    (enqueue-task! exec (get env k) v)
    (do (println env)
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn enqueue-emission! [exec {:keys [env msgs]}]
  (run! (fn [[k v]] (send! exec env k v)) msgs))

(defn run-task! [exec env task]
  ;; Tasks are contextual thunks, so they're functions of the env in which they
  ;; eventually execute.
  (let [v (task env)]
    (cond
      (ast/emission? v)                  (enqueue-emission! exec v)
      (ast/incomplete? v)                (println "error" v)
      (nil? v)                           nil
      ;; FIXME: using send here replaces a task with a task, which is no good.
      ;; At some point we have to forget the task stack and DO SOMETHING
      (contains? env (ast/xkey :return)) (send! exec env (ast/xkey :return) [v])
      true                               (println "dropping returned value: " v))))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec form]
  (enqueue! exec (walk-task form)))

(defn start! [exec root-cable]
  (let [env   (assoc root-cable ::new-task!
                     (builtins/primitive "enqueue" (fn [f args]
                                                     (enqueue-task! exec f args))))]
    (loop []
      (let [tasks (:work @exec)]
        (println (count tasks))
        (when (seq tasks)
          (println (peek tasks))
          ;; TODO: dosync for work stealing.
          (let [t (peek tasks)]
            ;; Remove task from work stack *before* running it!
            (swap! exec update :work pop)
            (run-task! exec env t))
          (recur))))))
