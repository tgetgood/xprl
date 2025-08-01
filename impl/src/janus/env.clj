(ns janus.env
  (:require
   [janus.ast :as ast]))

(def empty-ns
  {})

(defn ns-intern [ns sym val]
  (assoc ns (ast/unresolve sym) val))

(defn lookup [env sym]
  (get env sym))

(declare pin unpin)

(defn pin*
  "Walks `form` and resolves symbols found in `bindings`."
  [form env]
  (if (empty? env)
    form
    (cond
      (ast/resolved? form)   (update form :form pin env) ; don't recur in sym
      (ast/unresolved? form) (if (contains? env form)
                               (ast/resolve form (get env form))
                               form)

      (ast/μ? form)     (update form :body pin
                                (dissoc env (:params form) (:name form)))
      ;; ν
      (vector? form)    (mapv #(pin % env) form)
      (map-entry? form) [(pin (key form) env) (pin (val form) env)]
      (coll? form)      (reduce (fn [f x] (conj f (pin x env))) form form)
      true              form)))

(defn unpin*
  "Walks `form` and unresolves any occurances in `syms`"
  [form syms]
  (if (empty? syms)
    form
    (cond
      (ast/resolved? form)   (if (contains? syms form)
                               (ast/unresolve form)
                               (update form :form unpin syms))

      (ast/μ? form)     (update form :body unpin
                                (disj syms (:params form) (:name form)))
      ;; ν
      (vector? form)    (mapv #(unpin % syms) form)
      (map-entry? form) [(unpin (key form) syms) (unpin (val form) syms)]
      (coll? form)      (reduce (fn [f x] (conj f (unpin x syms))) form form)
      true              form)))

;; Without memoisation these are unusably slow.
(def pin (memoize pin*))
(def unpin (memoize unpin*))
