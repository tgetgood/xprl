(ns xprl.tag-fns
  (:refer-clojure :exclude [compile eval apply resolve])
  (:require [xprl.ast :as ast]))

(defn tag []
  (gensym "%"))

(declare fns)

(defn call [tag env args]
  (ast/call (get fns tag) env args))

(defn compile [env form]
  (cond
    (ast/immediate? form)   (let [f (tag) e (tag) c (tag)]
                              {:return (call :eval e c)
                               c       (call :compile e f)
                               f       (:form form)
                               e       env})
    (ast/application? form) (let [h (tag) h' (tag) t (tag) t' (tag) e (tag)]
                              {:return (call :apply e h' t')
                               h'      (call :compile e h)
                               h       (:head form)
                               t'      (call :compile e t)
                               t       (:tail form)
                               e       env})
    ;; TODO: emission, lists, maps, etc.
    true                    {:return form}))

(defn eval [env form]
  (cond
    (ast/ref? form)    {:return (:binding form)}
    (ast/symbol? form) (let [e (tag) s (tag)]
                         {:return (call :resolve e s)
                          e       env
                          s       form})
    (ast/pair? form)   (let [h (tag) t (tag) e (tag)]
                         {:return (call :compile e h)
                          h       (ast/application (ast/immediate (:head form)) (:tail form))
                          e       env})
    ;; TODO: data types
    true               {:return form}))


(defn concretise [env head tail]
  )

(defn apply [env head tail]
  (cond
    (ast/external? head) (let [[h t e] (repeatedly tag)]
                           {:return (ast/call h e t)
                            h       head
                            e       env
                            t       tail})
    (ast/μ? head)  (let [body (concretise env head tail)])
    true                 (assert false)))

(defn resolve [env sym]
  ;; weirdly here, resolution does *not* find values for names, rather it
  ;; decides whether a symbol stands for an unknown value or is just a symbol.
  ;; I'm not entirely sure what the latter means at the moment.
  ;; REVIEW: It's probably an error... but is it?
  (if (contains? (:captured env) sym)
    {:return (call :await-input (get-in env [:captured sym]))}
    (assert false (str "trying to resolve unbound symbol: " sym))))

(defn join [env & xs]
  (vec xs))

(defn createμ [env param body])

(defn μwrap [env routine tag])

(def fns
  ;; These are compiler internals that cannot currently be called at runtime.
  ;; That'll have to change.
  {:compile     compile
   :eval        eval
   :resolve     resolve
   :apply       apply
   :await-input (constantly nil)})
