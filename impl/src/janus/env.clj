(ns janus.env
  (:refer-clojure :exclude [name declare])
  (:require
   [clojure.set :as set]
   [janus.ast :as ast]
   [janus.debug :refer [trace!]]))


(def empty-ns
  {:names {} :declarations #{} :outer {}})

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

(defn switch? [x]
  (satisfies? ContextSwitch x))

(defrecord EnvWrapper [form ctx]
  janus.ast.Contextual
  ContextSwitch
  (merge-env [_ outer]
    (reduce (fn [e s]
              (if (contains? (names outer) s)
                (bind* e s (lookup outer s))
                e))
            ctx (decls ctx))))

(defrecord Declaration [form ctx]
  ;; If a message is being sent from beneath a declaration node, then the
  ;; receiver must be beneath the *same* declaration node. This preserves
  ;; reference to unknowns.
  janus.ast.Contextual
  ContextSwitch
  (merge-env [_ outer]
    (reduce declare* outer (decls ctx))))

(defrecord Binding [form ctx]
  janus.ast.Contextual
  ContextSwitch
  (merge-env [_ outer]
    (reduce (fn [acc [s v]] (bind* acc s v)) outer (names ctx))))

(defn pin [body env]
  (if (ast/contextual? body)
    (->EnvWrapper body env)
    body))

(defn declare [body & syms]
  (->Declaration body (reduce declare* empty-ns syms)))

(defn bind [form & bindings]
  ;; (assert (instance? janus.env.Declaration form) form)
  (let [env (reduce (fn [env [sym val]] (if (nil? sym) env (bind* env sym val)))
                    empty-ns (partition 2 bindings))]
    (if (= env empty-ns)
      form
      (->Binding form env))))

(defn rewrap [form ctx]
  (if (ast/contextual? form)
    (assoc ctx :form form)
    form))
