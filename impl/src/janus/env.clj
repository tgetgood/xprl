(ns janus.env
  (:refer-clojure :exclude [declare resolve])
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [janus.ast :as ast])
  (:import
   (java.io Writer)))

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
  (resolve [this])
  (reresolve [this]))

(defrecord DeclaredSymbol [symbol id]
  Object
  (toString [_]
    (str symbol "{" id "}"))

  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    #{symbol}))

(defrecord ResolvedSymbol [symbol binding]
  Object
  (toString [_]
    (str symbol "{=" binding "}"))

  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    #{symbol}))

(ast/ps ResolvedSymbol)

(defmethod pp/simple-dispatch ResolvedSymbol [o]
  (pp/write-out (str o)))

(defrecord Context [form ctx]
  Object
  (toString [_]
    (str "#C" (keys (names ctx)) "," (decls ctx) "::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (resolve [_]
    (if-let [binding (lookup ctx form)]
      (->ResolvedSymbol form binding)
      form))
  (reresolve [_]
    (if (contains? (decls ctx) (:symbol form))
      (:symbol form)
      form)))

(ast/ps Context)

(defrecord Declaration [form id syms]
  Object
  (toString [_]
    (str "#D" syms "::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (resolve [_]
    form)
  (reresolve [_]
    (if (contains? syms (:symbol form))
      (:symbol form)
      form)))

(ast/ps Declaration)

(defrecord Binding [form bindings]
  Object
  (toString [_]
    (str "#B" bindings "::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (resolve [_]
    (if-let [binding (get bindings form)]
      (->ResolvedSymbol form binding)
      form))
  (reresolve [_]
    form))

(ast/ps Binding)

(defmethod pp/simple-dispatch Context [{:keys [form ctx]}]
  (pp/write-out (str "#C" (keys (names ctx)) "," (decls ctx) "::"))
  (pp/simple-dispatch form))

(extend-protocol ast/Inspectable
  ResolvedSymbol
  (insp [{:keys [form]} ^Writer w level]
    (ast/spacer w level)
    (.write w "S*[")
    (.write w (str form))
    (.write w "]\n"))

  Context
  (insp [{:keys [form]} ^Writer w level]
    (ast/spacer w level)
    (.write w "C\n")
    (ast/insp form w (inc level)))

  Declaration
  (insp [{:keys [form syms]} ^Writer w level]
    (ast/spacer w level)
    (.write w "D")
    (.write w (str syms))
    (.write w "\n")
    (ast/insp form w (inc level)))

  Binding
  (insp [{:keys [form bindings]} ^Writer w level]
    (ast/spacer w level)
    (.write w "B")
    (.write w (str bindings))
    (.write w "\n")
    (ast/insp form w (inc level))))

(defn pin [body env]
  (if (ast/contextual? body)
    (->Context body env)
    body))

(defn declare [body id syms]
  (->Declaration body id (into #{} syms)))

(defn bind [body id bindings]
  (->Binding body id bindings))

(def type-table
  {Context        :C
   Declaration    :D
   Binding        :B})

(defn ctx? [x]
  (satisfies? ContextSwitch x))

(defn pushall [ctx form]
  (reduce (fn [acc [k v]] (assoc acc k (assoc ctx :form v))) form form))

(defn push-down [ctx]
  (let [inner (:form ctx)]
    (cond
      (ast/pair? inner)        (pushall ctx inner)
      (ast/application? inner) (pushall ctx inner)
      (ast/immediate? inner)   (pushall ctx inner)
      (ast/emission? inner)    (pushall ctx inner)
      (vector? inner)          (mapv #(assoc ctx :form %) inner)
      (ast/μ? inner)           (assoc inner :body (assoc ctx :form (:body inner)))
      true                     inner)))


;; REVIEW: Really going with the metaphor...
(defn bore [{{:keys [form ctx]} :form syms :syms}]
  (pin form (reduce declare* ctx syms)))


(defn filter-names [bindings decls]
  (into {} (filter #(contains? decls (key %))) bindings))

(defn merge-bind [ctx bindings]
  (reduce (fn [e [k v]] (bind* e k v)) ctx bindings))

(defn merge-decl [ctx decls]
  (reduce declare* ctx decls))

(defn merge-ctx [{:keys [ctx form]}]
  (let [outer-bindings (names ctx)
        outer-decls    (decls ctx)
        inner-bindings (names (:ctx form))
        inner-decls    (decls (:ctx form))
        inner-form     (:form form)

        bindings (into {} (concat (remove #(contains? outer-decls (key %))
                                          inner-bindings)
                                  (filter #(contains? inner-decls (key %))
                                          outer-bindings)))
        decls    (remove #(contains? bindings %)
                         (set/union inner-decls outer-decls))]
    (pin inner-form (assoc empty-ns :names bindings :declarations decls))))

#_(defn merge-ctx [{:keys [ctx form] :as outer}]
  (let [ictx (:ctx form)
        iform (:form form)
        tag [(t2 (type outer)) (t2 (type form))]]
    (case tag
      [:C :C] form

      [:C :D] (pin iform (merge-decl ctx ictx))
      [:D :C] (pin iform (merge-decl ictx ctx))

      [:C :B] (pin iform (merge-bind ctx ictx))
      [:B :C] (pin iform (merge-bind ictx (filter-names ctx (decls ictx))))

      [:B :B] (->Binding iform (merge ctx ictx))
      [:D :D] (->Declaration iform (set/union ctx ictx))

      ;; Binding and declaration happen at different points in the lifecycle of
      ;; a μ. But can the bindings of one abut the declarations of another? I'm
      ;; not positive they can't. I don't have a good intuition yet.
      [:B :D] ; remove inner decls if bound
      [:D :B] ; remove inner bindings if declared
      )))
