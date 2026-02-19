(ns xprl.tag-fns
  (:refer-clojure :exclude [compile eval apply resolve])
  (:require [xprl.ast :as ast]))

(defn tag []
  (gensym "%"))

(defmacro with-tags [names body]
  `(let [~names (repeatedly tag)]
     ~body))

(declare fns)

(defn call [tag & args]
  (ast/call (get fns tag) args))

(defn compile [[e f] vmap]
  (let [form (get vmap f)]
    (cond
      (ast/immediate? form) (with-tags [f c]
                              {:return (call :eval e c)
                               c       (call :compile e f)
                               f       (:form form)})
      (ast/application? form) (with-tags [h h' t t']
                                {:return (call :apply e h' t')
                                 h'      (call :compile e h)
                                 h       (:head form)
                                 t'      (call :compile e t)
                                 t       (:tail form)})
      ;; TODO: emission, lists, maps, etc.
      true                    {:return form})))

(defn eval [[e f] vmap]
  (let [form (get vmap f)]
    (cond
      (ast/ref? form)    {:return (:binding form)}
      (ast/symbol? form) {:return (call :resolve e f)}
      (ast/pair? form)   (with-tags [a]
                           {:return (call :compile e a)
                            ;; REVIEW: This is very non-idiomatic, but the
                            ;; alternative is copy paste of
                            ;; compile-application...
                            a       (ast/application (ast/immediate (:head form))
                                                     (:tail form))})
      ;; TODO: data types
      true               {:return form})))


(defn concretise [env head tail]
  ;; Implement createμ then worry about application
  )

(defn apply [[e h t] vmap]
  (let [head (get vmap h)]
    (cond
      (ast/external? head) {:return (ast/call head [e t])}
      (fn? head)           {:return (ast/call head [e t])}
      (ast/μ? head)  1 #_(let [body (concretise env head tail)])
      true                 (assert false (type head)))))

(defn resolve [[e s] vmap]
  ;; weirdly here, resolution does *not* find values for names, rather it
  ;; decides whether a symbol stands for an unknown value or is just a symbol.
  ;; I'm not entirely sure what the latter means at the moment.
  ;; REVIEW: It's probably an error... but is it?
  (let [env (get vmap e)
        sym (get vmap s)]
    (with-tags [id]
      (if (contains? (:captured env) sym)
        {:return (call :await-input s id)
         id      (get-in env [:captured sym])}
        (assert false (str "trying to resolve unbound symbol: " sym))))))


(defn capture [[e p id] vmap]
  (let [env   (get vmap e)
        param (get vmap p)
        μid   (get vmap id)]
    {:return (assoc-in env [:captured param] μid)}))

(defn μwrap [[id p b] vmap]
  (let [μid   (get vmap id)
        body  (get vmap b)
        param (get vmap p)]
    {:return (ast/μ id p b)}))

(defn createμ [[e a] vmap]
  (let [[param body] (get vmap a)]
    (with-tags [e' μid b b' p p']
      {:return (call :μwrap μid p' b')
       b'      (call :compile e' b)
       b       body
       p       param
       p'      (call :compile e p)
       μid     (gensym "μ-param-")
       e'      (call :capture e p μid)})))

(defn await-input [[s i] vmap]
  {:return (ast/input (get vmap s) (get vmap i))})

(defn +* [[a b] vmap]
  (+ (get vmap a) (get vmap b)))

(def fns
  ;; These are compiler internals that cannot currently be called at runtime.
  ;; That'll have to change.
  {:compile     compile
   :eval        eval
   :resolve     resolve
   :apply       apply
   :await-input await-input
   :μ           createμ
   :μwrap       μwrap
   :capture     capture
   :+*          +*})
