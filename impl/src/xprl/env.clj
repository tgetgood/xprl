(ns xprl.env
  ;; FIXME: This namespace is a trash heap of confused concepts.
  (:refer-clojure :exclude [bound? resolve])
  (:require
   [clojure.set :as set]
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

;; REVIEW: Is it permissible to block the binding of a symbol that has no bindings?
(defn popbind [x]
  (if (empty? x) x (pop x)))

(defn incorporate [env {:keys [bindings block?]}]
  (if block?
    (update env :bindings #(reduce (fn [b k] (update b k popbind)) % bindings))
    (update env :bindings #(reduce (fn [b [k v]] (update b k conj v)) % bindings))))

(defn μ-binding [{:keys [name params body] :as μ} args]
  (ast/lex (merge {params args} (when name {name μ})) body))

(defn bound? [{:keys [bindings]} sym]
  (let [s (ast/sym sym)]
    (when (contains? bindings s)
      (not (empty? (get bindings s))))))

(defn resolve [{:keys [bindings]} sym]
  (let [s   (ast/sym sym)
        res (peek (get bindings s))]
    (ast/block s res)))

(defn compact-bindings [{:keys [form bindings block?] :as lex}]
  (assoc lex :bindings
         (let [syms (ast/free-symbols form)]
           (if block?
             (reduce disj bindings (remove #(contains? syms %) bindings))
             (reduce dissoc bindings (remove #(contains? syms %) (keys bindings)))))))

(defn compact-nested [{ob :bindings {ib :bindings block? :block? :as il} :form :as ol}]
  ;; outer will never be blocked.
  (if block?
    (let [nob (reduce dissoc ob ib)
          nib (reduce disj ib (key ob))]
      (-> ol
          (assoc :bindings nob)
          (assoc-in [:form :bindings] nib)))
    ;; Inner bindings clobber outer bindings
    (update il :bindings #(merge ob %))))

(defn compact [x]
  (if (ast/lex? x)
    (let [{:keys [form bindings block?] :as lex} (compact-bindings x)]
      (cond
        (empty? bindings) (recur form)
        (ast/lex? form)   (recur (compact-nested lex))
        true              lex))
    x))

(defn walk-μ [env {:keys [name params]}]
  (-> env
      (assoc :μ? true)
      (update :bindings dissoc name params)))

(defn walk-channels [env {:keys [chs]}]
  (update env :ctx merge chs))

(defn anchor [env form]
  (ast/lex (into {} (comp (filter #(bound? env %))
                          (map (fn [x] [(ast/sym x) (resolve env x)])))
                 (ast/free-symbols form))
           form))

(defn capture [args]
  (let [names (mapv ast/sym (butlast args))]
    (when (every? ast/unresolved? names)
      (conj names (last args)))))

(defn flatten-lexical [form]
  (if (ast/lex? form)
    (let [{:keys [bindings form] :as lex} (compact form)]
      (if (vector? form)
        (mapv #(assoc lex :form %) form)
        lex))
    form))
