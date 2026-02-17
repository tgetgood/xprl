(ns xprl.tag-fns
  (:refer-clojure :exclude [compile eval apply resolve])
  (:require [xprl.ast :as ast]))

(defn tag []
  (gensym "%"))

(defn compile [env form]
  (cond
    (ast/immediate? form)   (let [f (tag) e (tag)]
                              {:return (ast/call :eval e f)
                               f       (:form form)
                               e       env})
    (ast/application? form) (let [h (tag) t (tag) e (tag)]
                              {:return (ast/call :apply e h t)
                               h       (:head form)
                               t       (:tail form)
                               e       env})
    ;; TODO: emission, lists, maps, etc.
    true                    {:return form}))

(defn eval [env form]
  (cond
    (ast/ref? form)    {:return (:binding form)}
    (ast/symbol? form) (let [e (tag) s (tag)]
                         {:return (ast/call :resolve e s)
                          e       env
                          s       form})
    ;; REVIEW: Is there any benefit to returning an application as a value?
    (ast/pair? form)   (let [h (tag) t (tag) e (tag)]
                         {:return (ast/call :apply e h t)
                          h       (ast/immediate (:head form))
                          t       (:tail form)
                          e       env})
    ;; TODO: data types
    true               {:return form}))

(defn apply [env head tail])

(defn resolve [env sym]
  ;; weirdly here, resolution does *not* find values for names, rather it
  ;; decides whether a symbol stands for an unknown value or is just a symbol.
  ;; I'm not entirely sure what the latter means at the moment.
  ;; REVIEW: It's probably an error... but is it?
  (if (contains? (:captured env) sym)
    {:return (ast/call :await-input (get-in env [:captured sym]))}
    (assert false (str "trying to resolve unbound symbol: " sym))))

(defn join [env & xs]
  (vec xs))

(defn createμ [env param body])

(defn μwrap [env routine tag])

(def fns
  {:compile compile
   :eval    eval
   :resolve resolve
   :apply   apply
   :join    join
   :vec     join})
