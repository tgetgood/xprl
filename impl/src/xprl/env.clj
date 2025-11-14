(ns xprl.env
  (:refer-clojure :exclude [bound? resolve])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

(defn strip
  "Removes lexical env from a form"
  [x]
  (dissoc x ::env))

(def empty-env ::root)

(defn local [x]
  (if-let [e (::env x)]
    e
    empty-env))

(defn local? [x]
  (not (nil? (::env x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-ns {})

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/unresolved? (keys ns)) ns)
  (walk/postwalk #(if (contains? ns %) (get ns %) %) body))

(defn ns-intern [ns sym val]
  (assert (ast/unresolved? sym) sym)
  (assoc ns (strip sym) val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Env Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn push [env frame]
  (assoc frame ::previous env))

;; Not reverse! something much weirder.
(defn invert [s]
  (if (= (::previous s) ::root)
    s
    (recur (assoc (::previous s) ::next s))))

(defn merge-stacks
  ;; AKA despaghettify
  "Given two stacks, find their common root and create a new stack of the form
  root<-unique part of `outer`<-unique part of `inner`."
  [inner outer]
  (cond
    (= inner outer)     inner ; this case would lead to infinite looping below
    (= empty-env inner) outer
    (= empty-env outer) inner
    true
    (loop [i (invert inner)
           o (invert outer)]
      (if (= (dissoc i ::next) (dissoc o ::next))
        (recur (::next i) (::next o))
        (cond ; the first two cases are just an optimisation.
          (nil? i) outer
          (nil? o) inner
          true
          (loop [root (::previous o)
                 o    o]
            (if (contains? o ::next)
              (recur (push root (dissoc o ::next)) (::next o))
              (loop [root (push root o)
                     i    i]
                (if (contains? i ::next)
                  (recur (push root (dissoc i ::next)) (::next i))
                  (push root i))))))))))

(defn with-env [env form]
  (cond
    (vector? form) (into [] (map (partial with-env env)) form)
    (map? form)    (assoc form ::env env)
    true           form))

;; OPTIMISE: This may benefit from memoisation.
(defn resolve [env {sym :form :as im}]
  (let [s   (strip (ast/sym sym))
        env (merge-stacks (local sym) env)]
    (loop [{:keys [bindings] :as env} env]
      (cond
        (= ::root env)         im
        (contains? bindings s) (let [next (get bindings s)] ; `next` might be `false`!
                                 (with-env (merge-stacks (local next) (::previous env))
                                   next))
        true                   (recur (::previous env))))))

(defmacro in-env [form env body]
  {:style/indent 2}
  `(let [~env (merge-stacks (local ~form) ~env)]
     ~body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; μ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn capture [args]
  (let [names (mapv strip (map ast/sym (butlast args)))]
    (when (every? ast/unresolved? names)
      (conj names (last args)))))

(defn bindargs
  [env {:keys [name params body] :as μ} args]
  (trace! "binding" (merge {params args} (when name {name μ}))
          "\nin\n" env "->" (merge-stacks (local body) env)
          "\nwith\nparams" (local args)
          "\nμ" (local μ))
  (let [binding {:bindings (merge {params args} (when name {name μ}))}]
    (with-env (push (merge-stacks (local body) env) binding) body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; test cases
;; TODO: real tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def f (reduce push empty-env [{:a 1} {:b 2} {:c 4}]))
;; (def g (reduce push empty-env [{:a 1} {:b 2} {:d 5} {:e 7}]))
;; (def g' (push g {:test 42}))

;; (assert (= f (merge-stacks f empty-env) (merge-stacks empty-env f)))
;; (assert (= (merge-stacks f f) f))
;; (assert (= (merge-stacks g g') (merge-stacks g' g) g'))

;; (assert (= 4 (:c (merge-stacks f g))))
;; (assert (= 7 (:e (merge-stacks g f))))
