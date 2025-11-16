(ns xprl.env
  (:refer-clojure :exclude [resolve extend])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

(defn sym-walk
  "Replace every Symbol s in `form` with (`f` s). Leaves the rest unchanged."
  [f form]
  (if (ast/symbol? form)
    (f form)
    (walk/walk (partial sym-walk f) identity form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-ns {})
(def empty-env [[]])

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/symbol? (keys ns)) ns)
  (sym-walk #(assoc % ::ns ns ::env empty-env) body))

(defn ns-intern [ns sym val]
  (assert (ast/symbol? sym) sym)
  (assoc ns (ast/symbol sym) val))

;; N.B.: This is used for tooling. Don't delete it.
(defn lookup [env sym]
  (assert (ast/symbol? sym) sym)
  (get env sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Env Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn push [sym frame]
  ;; Always update the last
  (update sym ::env
          (fn [eq] (update eq (dec (count eq))
                           (fn [es] (conj es frame))))))

(defn bind [bindings form]
  (sym-walk (fn [s] (push s {:bindings bindings})) form))

(defn capture [syms form]
  (sym-walk (fn [s] (push s {:capture (into #{} syms)})) form))

(defn uncapture [env s]
  (loop [n (dec (count env))]
    (if (< n 0)
      (throw (RuntimeException. (str "Resolved uncaptured variable: " s)))
      (if (contains? (:capture (nth env n)) s)
        (into (subvec env 0 n) (subvec env (inc n)))
        (recur (dec n))))))

(defn frame-lookup
  "Looks up `s` in *stack* `env`. Returns the resolved value as well as the
  remainder of the stack (minus a preceeding capture frame if applicable)."
  [env s]
  (loop [n (dec (count env))]
    (when (< 0 n)
      (let [frame (nth env n)]
        (if-let [caps (:capture frame)]
          (when-not (contains? caps s) ; If a symbol is captured, give up and come
            (recur (dec n)))           ; back later
          (let [bs (:bindings frame)]
            (if (contains? bs s)
              [(get bs env) (uncapture (subvec env 0 n) s)]
              (recur (dec n)))))))))

(defn env-lookup
  "Returns the first binding of `s` in the queue of `envs` as well as the
  remaining unconsumed environment. "
  [envs s]
  (when-not (empty? envs)
    (let [[v rem] (frame-lookup (first envs) s)]
      (if (nil? v)
        (recur (rest envs) s)
        [v (into [rem] (rest envs))]))))

(defn ns-lookup [ns s]
  (get ns s))

(defn extend [sym env]
  ;; Extensions go at the end of the queue, so they get tried last.
  (update sym ::env into env))

(defn resolve [{:keys [::env ::ns] sym :form :as im}]
  (let [s       (ast/symbol sym)
        [v env] (env-lookup env s)]
    (if (nil? v)
      (let [v (ns-lookup ns s)]
        (if (nil? v)
          ;; If sym is unbound in the current env, then all pinned stacks before
          ;; the last can be thrown away since they will never update and thus
          ;; can never influence anything downstream of this symbol.
          ;; The last stack, however, might effect the resolved value if it ends
          ;; up bound in the stack at a later point in time.
          ;; Ugh, that's confusing. Which more or less matches my understanding.
          (update im :form update ::env (fn [e] [(last e)]))
          v))
      (extend v env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; μ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn μ-prepare [args]
  (when (every? ast/symbol? (butlast args))
    (let [names (conj (into (map ast/symbol) (butlast args)))]
      (conj names (capture names (last args))))))

(defn bindargs
  [{:keys [name params body] :as μ} args]
  (trace! "binding" (merge {params args} (when name {name μ})) "\nin\n" body)
  (bind (merge {params args} (when name {name μ})) body))
