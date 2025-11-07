(ns xprl.env
  (:refer-clojure :exclude [bound? resolve])
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
  (assert (ast/unresolved? sym) sym)
  (assoc ns sym val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

(defn popbind [x]
  (if (empty? x)
    x
    (pop x)))

;; Curiously, it's best to always reset the entire set of bindings rather than
;; allow it to grow with scope.
(defn incorporate [env {:keys [bindings block?]}]
  (if block?
    (update env :bindings #(reduce (fn [b k] (update b k popbind)) % bindings))
    (update env :bindings #(reduce (fn [b [k v]] (update b k conj v)) % bindings))))

(defn bind [{:keys [name params body] :as μ} args]
  (ast/lex (merge {params args} (when name {name μ})) body))

(defn bound? [{:keys [bindings block?]} sym]
  (let [s (ast/sym sym)]
    (when (contains? bindings s)
      (or block? (not (empty? (get bindings s)))))))

(defn resolve [{:keys [bindings]} sym]
  (let [s   (ast/sym sym)
        res (peek (get bindings s))]
    (ast/block s res)))

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

(defn anchor [env form]
  (ast/lex (into {} (comp (filter #(bound? env %))
                          (map (fn [x] [(ast/sym x) (resolve env x)])))
                 (ast/free-symbols form))
           form))

(defn pushdown [x env]
  (let [pbind (fn [bi])])
  (cond
    (and (ast/lex? x) (empty? (:bindings x))) (:form x)
    (and (ast/lex? x) (:block? x))            (pushdown (:form x) (incorporate env x))

    (ast/lex? x)    (let [bs (into {} (map (fn [[k v]] [k (pushdown v env)])) (:bindings x))]
                      (pushdown (:form x) (incorporate env {:bindings bs})))
    (ast/μ? x)      (update x :body pushdown (walk-μ env x))
    (ast/symbol? x) (if (bound? env x) (ast/lex {(ast/sym x) (resolve env x)} x) x)
    (map-entry? x)  [(pushdown (key x) env) (pushdown (val x) env)]
    (record? x)     (reduce (fn [r x] (conj r (pushdown x env))) x x)
    (coll? x)       (into (empty x) (map #(pushdown % env)) x)
    true            x))
