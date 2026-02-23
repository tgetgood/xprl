(ns xprl.tag-fns
  (:refer-clojure :exclude [compile eval apply resolve])
  (:require [clojure.set :as set]
            [xprl.ast :as ast]))

(defn tag []
  (gensym "%"))

(defmacro with-tags [names body]
  `(let [~names (repeatedly tag)]
     ~body))

(declare fns)


(defn call [tag & args]
  (ast/call (get fns tag) args))

(def compile-impls
  {ast/immediate?   (with-tags [f f' c i e]
                      {:return (call :eval e c)
                       c       (call :compile e f')
                       f'      (call :get f :form)
                       f       (call :nth i 2)
                       e       (call :nth i 1)
                       i       :input})
   ast/application? (with-tags [f h h' t t' e i]
                      {:return (call :apply e h' t')
                       h'      (call :compile e h)
                       h       (call :head f)
                       t'      (call :compile e t)
                       t       (call :tail f)
                       f       (call :nth i 2)
                       e       (call :nth i 1)
                       i       :input})})

(defn compile [[e f] vmap]
  (let [form (get vmap f)]
    (cond
      (ast/immediate? form)
      (ast/application? form)
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


(defn capture [env sym id]
  (assoc-in env [:captured sym] id))

(defn createμ [[e a] vmap]
  (let [[param body] (get vmap a)
        env          (get vmap e)
        b            (tag) e' (tag)
        μid          (gensym "μ-param-")
        cenv         (capture env param μid)
        subr         (compile [e' b] {e' cenv b body})]
    {:return (ast/μ μid param (assoc subr e' cenv))}))

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
   :+*          +*})
