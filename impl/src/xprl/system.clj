(ns xprl.system
  (:require [xprl.ast :as ast]
            [xprl.executor :as exec]))

(defn naked-cable [ks]
  (into {} (map (fn [k] [(ast/xkey k) (ast/wire (gensym (str (name k) "-repl-")))])) ks))

(defn base-cable []
  (naked-cable [:return :error :env :unbound :log]))

(defn splice! [wire tag connection]
  (swap! (:connections wire) assoc tag connection))

(defn splice [form conts tag]
  (let [cable (:cable (meta form))]
    (run! (fn [[k v]]
            (when (contains? cable k)
              (splice! (get cable k) tag v)))
          conts))
  form)

(defn init! []
  {:executors [(exec/create!)]})

;; FIXME: We should reuse the system, not recreate it for each form.
(defn start! [cable form]
  (let [sys (init!)]
    (exec/seed! (first (:executors sys)) form)
    (run! #(exec/start! % cable) (:executors sys))
    sys))
