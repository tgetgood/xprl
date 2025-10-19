(ns xprl.compiler
  (:require
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.system :as sys]))

(defn step [form]
    (walk/prewalk i/walk form))

(defn entry [ns form]
  (let [ctx (env/set-ns ns form)]
    (step ctx)))
