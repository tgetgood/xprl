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

(def empty-env [])

(defn local [x]
  (if-let [e (::env x)]
    e
    empty-env))

(defn local? [x]
  (not (nil? (::env x))))

(defn push [env frame]
  (conj env (assoc frame ::id (gensym))))

(defn merge-stacks [inner outer]
  (let [index (into #{} (map ::id) inner)]
    (into (into [] (remove #(contains? index (::id %))) outer) inner)))

(defn with-env [env form]
  (cond
    (vector? form) (into [] (map (partial with-env env)) form)
    (map? form)    (assoc form ::env env)
    true           form))

;; OPTIMISE: This may benefit from memoisation.
(defn resolve [env {sym :form :as im}]
  (let [s   (strip (ast/sym sym))
        env (merge-stacks (local sym) env)]
    (loop [n (dec (count env))]
      (if (< n 0)
        im
        (let [frame (nth env n)]
          (if (contains? frame s)
            (let [next (get frame s)] ; `next` might be `false`!
              (with-env (merge-stacks
                         (local next)
                         (into (subvec env 0 n) (map #(select-keys % [::id]))
                               (subvec env n)))
                next))
            (recur (dec n))))))))

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
  (let [binding (merge {params args} (when name {name μ}))]
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
;; (assert (not= (merge-stacks g' g) (merge-stacks g g')))
;; (assert (= (merge-stacks g' g) g'))

;; (assert (= 4 (:c (last (merge-stacks f g)))))
;; (assert (= 7 (:e (last (merge-stacks g f)))))
