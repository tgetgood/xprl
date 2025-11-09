(ns xprl.env
  (:refer-clojure :exclude [bound? resolve])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

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
  (assoc ns sym val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Lexical env in AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-env [outer inner]
  (if (nil? inner)
    outer
    (merge outer
           {:bindings (merge (reduce dissoc (:bindings outer) (:blocks inner))
                             (:bindings inner))
            :blocks   (into (or (:blocks outer) #{})
                            (remove #(contains? (:bindings outer) %))
                            (:blocks inner))})))

(defn attach [form env]
  (trace! "incorporating" (::lex form) "into" env)
  (let [env (merge-env env (::lex form))]
    (trace! "->" env)
    (cond
      (empty? env) form
      (vector? form)  (mapv #(attach % env) form)
      (record? form)  (assoc form ::lex env)
      (map? form)     (into {} (map (fn [[k v]] [(attach k env) (attach v env)]) form))
      true            form)))

(defonce tt (atom nil))

(defn bindμ [{:keys [name params body] :as μ} args]
  (trace! "binding μ:" (merge {params args} (when name {name μ})))
  (reset! tt {:in {params args}
              :out (attach body {:bindings (merge {params args} (when name {name μ}))})})
  (attach body {:bindings (merge {params args} (when name {name μ}))}))

(defn block [form & syms]
  (trace! "blocking:" syms)
  (attach form {:blocks (set syms)}))

(defn capture [args]
  (let [names (mapv ast/sym (butlast args))]
    (when (every? ast/unresolved? names)
      (conj names (last args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Dynamic env During Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-env {:bindings {} :ctx {}})

(defn walk-μ [env {:keys [name params]}]
  (-> env
      (assoc :μ? true)
      (update :bindings dissoc name params)))

(defn walk-channels [env {:keys [chs]}]
  (update env :ctx merge chs))

;; REVIEW: Is it permissible to block the binding of a symbol that has no bindings?
(defn popbind [m k]
  (update m k #(if (empty? %) % (pop %))))

(defn pushbind [m [k v]]
  (update m k conj v))

(defn incorporate [form env]
  (if-let [local (::lex form)]
    (update env :bindings #(as-> % bindings
                             (reduce popbind bindings (:blocks local))
                             (reduce pushbind bindings (:bindings local))))
    env))

(defn bound? [{:keys [bindings]} sym]
  (let [s (ast/sym sym)]
    (when (contains? bindings s)
      (not (empty? (get bindings s))))))

(defn resolve [{:keys [bindings]} sym]
  (let [s   (ast/sym sym)
        res (peek (get bindings s))]
    (block res s)))

(defn popall [env]
  {:bindings (into {} (comp (map (fn [[k v]] (when-not (empty? v)
                                               [k (peek v)])))
                            (remove nil?))
                   (:bindings env))
   :blocks   (:blocks env)})

(defmacro propagate [form env body]
  {:style/indent 2}
  `(let [~env (incorporate ~form ~env)]
     (attach ~body (popall ~env))))
