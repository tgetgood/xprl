(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.builtins :as builtins]
            [xprl.env :as env]
            [xprl.interpreter :as i]))

(defn walk-task [env form]
  [i/walk [env form]])

(defn μ-task [env μ args]
  (let [bindings {(:id μ)  args
                  (:rec μ) μ}]
    (walk-task [env (env/invoke bindings (:body μ))])))

(defn external-task [env f args]
  [ast/call [env f args]])

(defn task [env f args]
  (cond
    (ast/μ? f)        (μ-task env f args)
    (ast/external? f) (external-task env f args)
    true              (throw (RuntimeException. (str "cannot enqueue " f)))))

(defn enqueue! [exec task]
  (swap! exec update :work conj task))

(defn add-work! [exec env f args]
  (enqueue! exec (task env f args))
  nil)

(defn send! [exec {:keys [cable] :as env} [k v]]
  (if (= ::new-task! k) ; HACK: Hard coded channel to prevent circular dependencies.
    (apply add-work! exec v)
    (if (contains? cable k)
      (add-work! exec env (get cable k) v)
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn enqueue-emission! [exec env {:keys [msgs] :as em}]
  (let [env (update env :cable :merge (:cable (:env em)))]
    (run! (partial send! exec env) msgs)))

(defn run-task! [exec [f args]]
  ;; KLUDGE: first arg is always `env`. That should be better controlled.
  (let [env      (first args)
        args     (into [env] (rest args))
        v        (apply f args)]
    (if (ast/emission? v)
      (enqueue-emission! exec env v)
      (if (contains? (:cable env) (ast/xkey :return))
        (send! exec env [(ast/xkey :return) v])
        (println "dropping returned value: " v)))))

(defn create! []
  (atom {:work  []
         :index {}}))

(defn seed! [exec cable form]
  (enqueue! exec (walk-task {:cable cable} form)))

(defn start! [exec]
  (let [tasks (:work @exec)]
    (println (count tasks))
    (when (seq tasks)
      ;; TODO: dosync for work stealing.
      (let [t (peek tasks)]
        ;; Remove task from work stack *before* running it!
        (swap! exec update :work pop)
        (run-task! exec t))
      (recur exec))))
