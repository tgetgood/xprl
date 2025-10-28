(ns xprl.env
  (:require
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

(def empty-ns
  {})

(defn strip [m {:keys [name params]}]
  (apply dissoc m (map ast/sym (remove nil? [name params]))))

(defn rep [subs] (fn [form] (if (contains? subs form) (get subs form) form)))

(defn crep [subs]
  (fn [form]
    (let [s (ast/sym form)]
      (if (and (not (nil? s)) (contains? subs s))
        (get subs s)
        form))))

(defn sym-replace [form repfn subs]
  (cond
    (empty? subs) form
    (ast/μ? form) (update form :body sym-replace repfn (strip subs form))
    true          (walk/walk #(sym-replace % repfn subs) (repfn subs) form)))

(defn ns-intern [ns sym val]
  (assert (ast/unresolved? sym) sym)
  (assoc ns sym val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/unresolved? (keys ns)) ns)
  (sym-replace body rep (into {} (map (fn [[k v]] [k (ast/resolved k v)])) ns)))

(defn capture [args]
  (let [syms (mapv ast/capture (butlast args))
        subs (apply hash-map (interleave (map ast/unresolve (butlast args)) syms))]
    (trace! "capture" subs)
    (conj syms (sym-replace (last args) crep subs))))

(defn bind [{:keys [name params body] :as μ} args]
  (let [subs (merge {params (ast/resolve params args)}
                    (when name {name (ast/resolve name μ)}))]
    (trace! "bind" subs)
    (assert (every? ast/captured? (keys subs)))
    (sym-replace body rep subs)))
