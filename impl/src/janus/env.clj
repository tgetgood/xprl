(ns janus.env
  (:refer-clojure :exclude [declare resolve])
  (:require
   [janus.ast :as ast]))

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

(defrecord ResolvedSymbol [symbol binding]
  Object
  (toString [_]
    (str sym "{=" binding "}"))

  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [this]
    #{this}))

(defprotocol ContextSwitch
  (resolve [this])
  (reresolve [this]))

(defrecord CarriedEnvironment [form ctx]
  Object
  (toString [_]
    (str "#C::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (resovle [_]
    (if-let [binding (lookup ctx form)]
      (->ResolvedSymbol form binding)
      form))
  (reresolve [_] form))

(defrecord Binding [form bindings]
  ;; If a message is being sent from beneath a declaration node, then the
  ;; receiver must be beneath the *same* declaration node. This preserves
  ;; reference to unknowns.
  Object
  (toString [_]
    (str "#B::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (resolve [_]
    (if-let [binding (get bindings form)]
      (->ResolvedSymbol form binding)
      form))
  (reresolve [_] form))

(defrecord Declaration [form syms]
  Object
  (toString [_]
    (str "#B::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (resolve [_] form)
  (reresolve [_]
    (if (contains? syms (:symbol form))
      (:symbol form)
      form)))

(ast/ps CarriedEnvironment)
(ast/ps Declaration)
(ast/ps Binding)
(ast/ps ResolvedSymbol)

(defn pin [body env]
  (if (ast/contextual? body)
    (->CarriedEnvironment body env)
    body))

(defn declare [body & syms]
  (->Declaration body (into #{} (filter ast/symbol?) syms)))

(defn bind [form & bindings]
  (->Binding form (into {} (filter #(ast/symbol? (key %))) (partition 2 bindings))))

(def type-table
  ;; REVIEW: This is an odd form of polymorphism...
  {CarriedEnvironment :C
   Binding            :C
   Declaration        :C
   ResolvedSymbol     :R})

(defn ctx? [x]
  (satisfies? janus.env.ContextSwitch x))

(defn peel
  "Removes ns nodes recursively until we reach an ast node."
  [f]
  (if (ctx? f)
    (recur (:form f))
    f))

(defn asskeys [f ctx & keys]
  (reduce (fn [f k] (assoc f k ctx)) f ctx))

(defn pushall [ctx form]
  (reduce (fn [acc [k v]] (assoc acc k (assoc ctx :form v))) form form))

(defn push-down [ctx]
  (let [inner (:form ctx)]
    (cond
      (ast/pair? inner)        (pushall ctx inner)
      (ast/application? inner) (pushall ctx inner)
      (ast/immediate? inner)   (pushall ctx inner)
      (ast/emission? inner)    (pushall ctx inner)

      (vector? inner) (mapv #(assoc ctx :form %) inner)
      (ast/μ? inner)  (assoc inner :body (assoc ctx :form (:body inner))))))
