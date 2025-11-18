(ns xprl.env
  (:refer-clojure :exclude [resolve extend])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

(defn sym-walk
  "Replace every Symbol s in `form` with (`f` s). Leaves the rest unchanged."
  [f form]
  (if (ast/symbol? form)
    (do
      (trace! "update sym env for" form ":" (::env form) "->" (::env (f form)))
      (f form))
    (walk/walk (partial sym-walk f) identity form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-ns {})
(def empty-env [])

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/symbol? (keys ns)) ns)
  (sym-walk #(assoc % ::ns ns ::env empty-env) body))

(defn ns-intern [ns sym val]
  (assert (ast/symbol? sym) sym)
  (assoc ns (ast/symbol sym) val))

;; N.B.: This is used for tooling. Don't delete it.
(defn lookup [env sym]
  (assert (ast/symbol? sym) sym)
  (get env sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Env Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn push [sym frame]
  (update sym ::env conj frame))

(defn bind [bindings form]
  (sym-walk (fn [s] (push s {:bindings bindings})) form))

(defn capture [syms form]
  (sym-walk (fn [s] (push s {:capture (into #{} syms)})) form))

(defn frame-index
  "Looks up `s` in *stack* `env`. Returns the resolved value as well as the
  remainder of the stack (minus a preceeding capture frame if applicable)."
  [env s]
  (loop [n (dec (count env))]
    (when (< 0 n)
      (let [frame (nth env n)]
        (if-let [caps (:capture frame)]
          (when-not (contains? caps s) ; If a symbol is captured, give up and come
            (recur (dec n)))           ; back later
          (let [bs (:bindings frame)]
            (if (contains? bs s)
              n
              (recur (dec n)))))))))

(defn extend [symbol extra]
  (if-let [env (::env symbol)]
    (assoc symbol ::env (into extra env))
    symbol))

(defn resolve [{sym :form :as im}]
  (let [s   (ast/symbol sym)
        env (::env sym)
        n   (frame-index env s)]
    (if (nil? n)
      (let [v (get (::ns sym) s)]
        (if (nil? v) im v))
      (let [v     (get (:bindings (nth env n)) s)
            extra (subvec env (inc n))]
        (sym-walk #(extend % extra) v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; μ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn μ-prepare [args]
  (when (every? ast/symbol? (butlast args))
    (let [names (into [] (map ast/symbol) (butlast args))]
      (conj names (capture names (last args))))))

(defn bindargs
  [{:keys [name params body] :as μ} args]
  (trace! "binding" (merge {params args} (when name {name μ})) "\nin\n" body)
  (assert (not-any? #(contains? % ::env) [name params]))
  (bind (merge {params args} (when name {name μ})) body))
