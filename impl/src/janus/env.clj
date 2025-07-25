(ns janus.env
  (:refer-clojure :exclude [declare resolve])
  (:require
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

(defrecord ResolvedSymbol [symbol binding]
  Object
  (toString [_]
    (str symbol "{=" binding "}"))

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
  (resolve [_]
    (if-let [binding (lookup ctx form)]
      (->ResolvedSymbol form binding)
      form))
  (reresolve [_] form))

(defrecord Binding [form bindings]
  Object
  (toString [_]
    (str "#B" (keys bindings) "::" form))
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
  ;; If a message is being sent from beneath a declaration node, then the
  ;; receiver must be beneath the *same* declaration node. This preserves
  ;; reference to unknowns.
  Object
  (toString [_]
    (str "#D" (seq syms) "::" form))
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

(defn bind [body & bindings]
  (->Binding body (into {} (filter #(ast/symbol? (key %))) (partition 2 bindings))))

(def type-table
  ;; REVIEW: This is an odd form of polymorphism...
  {CarriedEnvironment :C
   Binding            :C
   Declaration        :C
   ResolvedSymbol     :R})

(defn ctx? [x]
  (satisfies? ContextSwitch x))

(defn peel
  "Removes ns nodes recursively until we reach an ast node."
  [f]
  (if (ctx? f)
    (recur (:form f))
    f))

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
      (ast/μ? inner)  (assoc inner :body (assoc ctx :form (:body inner)))

      true inner)))

(defn bind** [env [k v]]
  (bind* env k v))

(def merge-ctx-rules
  {
   ;; How could two full context switches stack up like this?
   [:C :C] (constantly (assert false)) ; This should never occur... I think

   ;; Once inside a context switch, bindings and declarations work normally.
   [:C :D] #(pin (:form (:form %)) (declare* (:ctx %) (-> % :form :ctx :syms)))
   [:C :B] (fn [{:keys [form ctx]}]
             (pin (:form form) (reduce bind** ctx (:bindings (:ctx form)))))

   ;; Declarations just stack up.
   [:D :D] (fn [{:keys [ctx form]}]
             (declare (:form form) (set/union (:syms ctx) (:syms (:ctx form)))))
   ;; Declarations outside of a context switch do nothing
   [:D :C] :form ; discard outer decls since syms can't occur in inner form.
   ;; Declarations outside of a binding can't override bindings, but
   ;; declarations of symbols not in the binding should propagate.
   [:D :B] (fn [{:keys [form ctx]}]
             (pin (:form form)
                  (as-> empty-ns %
                    (reduce declare* % (:syms ctx))
                    (reduce bind** % (:bindings (:ctx form))))))

   [:B :B] (fn [{:keys [ctx form]}]
             (update-in form [:ctx :bindings] #(merge (:bindings ctx) %)))
   ;; Bindings on the outside fill in declarations on the inside.
   [:B :C] fill-decls

   ;; FIXME: This is wrong. Binding on the outside should fill in declarations on the inside
   [:B :D] (fn [{:keys [form ctx]}]
             (pin (:form form)
                  (as-> empty-ns %
                    (reduce bind** % (:bindings form))
                    (reduce declare* % (:syms (:ctx form))))))})
