(ns xprl.env
  (:require
   [clojure.walk :as walk]
   [xprl.ast :as ast]))

(def empty-ns
  {})

(defn sym-replace [subs form]
  (let [f   (fn [x] (if (contains? subs x) (get subs x) x))
        rec (partial sym-replace subs)]
    (if
        (ast/resolved? form) (f form) ; DON'T walk into resolved values.
        (walk/walk rec f form))))

(defn ns-intern [ns sym val]
  (assoc ns (ast/unresolve sym) val))

(defn lookup [env sym]
  (get env sym))

(defn ast-replace [subs form]
  (walk/postwalk (fn [x] (if (contains? subs x) (get subs x) x)) form))

(defn set-ns [ns body]
  (sym-replace (into {} (map (fn [[k v]] [k (ast/resolved k v)])) ns) body))

(defn capture [args]
  (let [syms (mapv ast/capture (butlast args))
        subs (apply hash-map (interleave (butlast args) syms))]
    (conj syms (sym-replace subs (last args)))))

(defn bind [{:keys [name params body] :as μ} args]
  (let [subs (merge {params (ast/resolve params args)}
                    (when name {name (ast/resolve name μ)}))]
    (sym-replace subs body)))
