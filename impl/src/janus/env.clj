(ns janus.env
  (:refer-clojure :exclude [name declare])
  (:require
   [clojure.set :as set]
   [janus.ast :as ast]
   [janus.debug :refer [trace!]]))


(def empty-ns
  {:names {} :declarations #{}})

(defn names [env]
  (or (:names env) {}))

(defn decls [env]
  (:declarations env))

(defn declare* [env sym]
  (if (nil? sym)
    env
    (-> env
        (update :names dissoc sym)
        (update :declarations conj sym))))

(defn bind* [env sym val]
  (-> env
      (update :names assoc sym val)
      (update :declarations disj sym)))

(defn lookup [env sym]
  (get-in env [:names sym]))

(defn project
  "Fits `env` by removing all names not mentioned in `form`. Keeps
  declarations."
  [env form]
  (let [syms (ast/symbols form)]
    (transduce  (filter #(contains? syms %))
                (fn [env sym] (update env :names dissoc sym))
                env
                (keys (names env)))))

;;;;; Contexts

(defprotocol ContextSwitch
  (merge-env [inner outer]))

(defrecord EnvWrapper [form ctx]
  ContextSwitch
  (merge-env [_ outer]
    ;; When passing into a wrapped env, we allow definitions in the outer
    ;; environment to fulfill declarations in the wrapped environment.
    ;; REVIEW: This probably isn't right. We need to deal with undeclared and
    ;; unbound symbols.
    ;;
    ;; Should a wrapper even have declarations? A message is sent in context
    ;; which means that a message can't *be* sent until it has a context.
    ;;
    ;; But what about inside of nested μs? If a message is passed to an inner μ
    ;; which refers to a parameter from an outer μ then it will be passed with a
    ;; declaration. Or it ought to be.
    (reduce (fn [env s]
              (if-let [v (lookup (names outer) s)]
                (bind* env s v)
                env))
            ctx (decls ctx))))

(defrecord Declaration [form ctx]
  ;; If a message is being sent from beneath a declaration node, then the
  ;; receiver must be beneath the *same* declaration node. This preserves
  ;; reference to unknowns.
  ContextSwitch
  (merge-env [_ outer]
    (reduce declare* outer (decls ctx))))

(defrecord Binding [form ctx]
  ContextSwitch
  (merge-env [_ outer]
    (reduce (fn [acc [s v]] (bind* acc s v)) outer (names ctx))))

(defn pin [body env]
  (->EnvWrapper body env))

(defn declare [body & syms]
  (->Declaration body (reduce declare* empty-ns syms)))

(defn bind [form & bindings]
  ;; TODO: `form` should always be a Declaration in a Binding and the names
  ;; should match. Check that.
  (let [env (reduce (fn [env [sym val]] (if (nil? sym) env (bind* env sym val)))
                    empty-ns (partition 2 bindings))]
    (if (= env empty-ns)
      form
      (->Binding form env))))

(defn switch? [x]
  (instance? janus.env.ContextSwitch x))

(defn rewrap [form ctx]
  (assoc ctx :form form))
