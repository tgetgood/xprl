(ns xprl.system
  (:require [xprl.ast :as ast]))

(defn naked-cable [ks]
  (into {} (map (fn [k] [(ast/xkey k) (ast/wire (gensym (str (name k) "-repl-")))])) ks))

(defn base-cable []
  (naked-cable [:return :error :env :unbound :log]))
