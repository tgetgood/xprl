(ns xprl.env
  (:refer-clojure :exclude [bound?])
  (:require
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

;; TODO: Rewrite this entire ns. It's just a mess.
(def empty-ns
  {})

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/unresolved? (keys ns)) ns)
  (walk/postwalk #(if (contains? ns %) (get ns %) %) body))

(defn ns-intern [ns sym val]
  (assert (ast/unresolved? sym) sym)
  (assoc ns sym val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

(defn incorporate [env {:keys [bindings]}]
  (update env :bindings merge bindings))

(defn bind [{:keys [name params body] :as μ} args]
  (ast/lex (merge {params args} (when name {name μ})) body))

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

(defn mdiff [m1 m2]
  (reduce dissoc m1 (keys m2)))

;; HACK: I can't think of any other reasonably compact way to avoid walking the
;; entire tree every time...
(def hackee (proxy [Exception] ["break!"]))

(defn bound?* [bindings form]
  (cond
    (empty? bindings)  form
    (ast/lex? form)    (bound?* (mdiff bindings (:bindings form)) (:form form))
    (ast/symbol? form) (when (contains? bindings (ast/sym form))
                         (throw hackee))
    (ast/μ? form)      (bound?* (dissoc bindings (:name form) (:params form))
                                (:body form))
    (map-entry? form)  [(bound?* bindings (key form)) (bound?* bindings (val form))]
    (coll? form)       (map (partial bound?* bindings) form)))

(defn bound? [{:keys [bindings form]}]
  (try
    (bound?* bindings form)
    false
    (catch Exception e
      (if (= e hackee)
        true
        (throw e)))))
