(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.executor :as exec]))

(defn init! []
  {:executors [(exec/create!)]})

;; FIXME: We should reuse the system, not recreate it for each form.
(defn start! [cable form]
  (let [sys (init!)]
    (exec/seed! (first (:executors sys)) cable form)
    (run! exec/start! (:executors sys))
    sys))
