(ns xprl.env
  (:refer-clojure :exclude [bound?])
  (:require
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

;; TODO: Rewrite this entire ns. It's just a mess.
(def empty-ns {})

(def empty-env {:bindings {} :ctx {}})

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/unresolved? (keys ns)) ns)
  (walk/postwalk #(if (contains? ns %) (get ns %) %) body))

(defn ns-intern [ns sym val]
  (assert (ast/unresolved? sym) (ast/incomplete? sym))
  (assoc ns sym val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

;; Curiously, it's best to always reset the entire set of bindings rather than
;; allow it to grow with scope.
(defn incorporate [env {:keys [bindings]}]
  (assoc env :bindings bindings))

(defn bind [bindings {:keys [name params body] :as μ} args]
  (ast/lex (merge bindings {params args} (when name {name μ})) body))

(defn walk-μ [env {:keys [name params]}]
  (-> env
      (assoc :μ? true)
      (update :bindings dissoc name params)))

(defn walk-channels [env {:keys [chs]}]
  (update env :ctx merge chs))

(defn capture [args]
  ;; The name and params of a μ must be naked symbols. It doesn't make sense for
  ;; them to refer to anything until the μ receives arguments.
  (conj (mapv ast/unresolve (butlast args)) (last args)))
