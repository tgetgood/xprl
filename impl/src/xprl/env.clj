(ns xprl.env
  (:require
   [clojure.walk :as walk]
   [xprl.ast :as ast]))

(def empty-ns
  {})

(defn ns-intern [ns sym val]
  (assoc ns (ast/unresolve sym) val))

(defn lookup [env sym]
  (get env sym))

(defn ast-replace [subs form]
  (walk/postwalk (fn [x] (if (contains? subs x) (get subs x) x)) form))

(defn ns-set! [ns body]
  (let [binds (into {} (map (fn [[k v]] [k (ast/->Resolved k :ns v)])) ns)]
    (ast-replace binds body)))

(defn capture [args]
  (let [[name params body] (if (= 3 (count args)) args (into [nil] args))
        p                  (ast/capture params)
        n                  (when name (ast/capture name))]
    [n p (ast-replace (merge {params p} (when name {name n})) body)]))

(defn bind [{:keys [name params body] :as μ} args]
  (let [subs (merge {params (ast/resolve params args)}
                    (when name {name (ast/resolve name μ)}))]
    (with-meta (ast-replace subs body) (meta μ))))
