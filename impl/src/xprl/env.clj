(ns xprl.env
  (:refer-clojure :exclude [resolve extend])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sym-walk
  "Replace every Symbol in `form` by a namespaced Ref (to `ns`)."
  [ns form]
  (if (and (ast/symbol? form) (contains? ns form))
    (ast/ref form (get ns form))
    (walk/walk (partial sym-walk ns) identity form)))

(def empty-ns {})

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/symbol? (keys ns)) ns)
  (sym-walk ns body))

(defn ns-intern [ns sym val]
  (assoc ns (ast/symbol sym) val))

;; N.B.: This is used for tooling. Don't delete it.
(defn lookup [env sym]
  (assert (contains? env sym) sym)
  (get env sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; μ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replacing symbols one at a time is inefficient, but so much clearer that for
;; now I'm sticking with it.
(defn walk-replace [key val body]
  (cond
    (and (ast/μ? body) (or (= (:params body) (ast/symbol key))
                           (= (:name body) (ast/symbol key))))
    body
    (= body key)    val
    ;; Don't replace the :sym key of a Ref!!!
    (ast/ref? body) (update body :bindings #(walk-replace key val %))
    true            (walk/walk (partial walk-replace key val) identity body)))

(defn capture [body name]
  (walk-replace name (ast/symbol name) body))

(defn μ-prepare [args]
  (let [names (butlast args)]
    (when (every? ast/symbolic? names)
      (conj (mapv ast/symbol names) (reduce capture (last args) names)))))

(defn bind [body [k v]]
  (walk-replace k (ast/ref k v) body))

(defn bindargs
  [{:keys [name params body] :as μ} args]
  (trace! "binding" (merge {params args} (when name {name μ})) "\nin\n" body)
  (assert (and (ast/symbol? params) (or (nil? name) (ast/symbol? name))))
  (reduce bind body (merge {params args} (when name {name μ}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Resolution (almost trivial at this point)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn resolve [{sym :form :as im}]
  (cond
    (ast/symbol? sym) im
    (ast/ref? sym)    (:binding sym)
    true              (assert false "unreachable!")))
