(ns xprl.system
  (:require [xprl.ast :as ast]))

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
