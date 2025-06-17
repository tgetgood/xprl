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
    (ast/symbols form)))

(ast/ps Context)

(defmethod pp/simple-dispatch Context [{:keys [form ctx]}]
  (pp/write-out (str "#C" (keys (names ctx)) "," (decls ctx) "::"))
  (pp/simple-dispatch form))

(extend-protocol ast/Inspectable
  ResolvedSymbol
  (insp [{:keys [form]} ^Writer w level]
    (ast/spacer level)
    (.write w "S*[")
    (.write w (str form))
    (.write w "]\n"))

  Context
  (insp [{:keys [form]} ^Writer w level]
    (ast/spacer level)
    (.write w "C\n")
    (ast/insp form w (inc level))))

(defn pin [body env]
  (if (ast/contextual? body)
    (->Context body env)
    body))

(defn declare [body & syms]
  (pin body (reduce declare* empty-ns (filter ast/symbol? syms))))

(defn bind [body & bindings]
  (pin body (reduce (fn [e [k v]] (bind* e k v)) empty-ns
                    (filter (fn [[k _]] (ast/symbol? k))
                            (apply hash-map bindings)))))

(defn resolve [{:keys [form ctx]}]
  (if-let [binding (lookup ctx form)]
      (->ResolvedSymbol form binding)
      form))

(defn reresolve [{:keys [form ctx]}]
  (if (contains? (decls ctx) (:symbol form))
      (:symbol form)
      form))

(def type-table
  {Context        :C
   ResolvedSymbol :R})

(defn ctx? [x]
  (instance? Context x))

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
