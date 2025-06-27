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
    (transduce (remove #(contains? syms %))
               (completing (fn [env sym] (update env :names dissoc sym)))
                env
                (keys (names env)))))

(defn shadow [{inner :form :as outer}]
  (if (= (:syms inner) (:syms outer))
    (assoc outer :form (:form inner))
    outer))

(defn resolve [{{ctx :ctx sym :form} :form :as im}]
  (if-let [v (lookup ctx sym)]
    v
    (assoc im :form sym)))

(defn c-or-d [{{sym :form syms :syms :as decl} :form :as ctx}]
  (if (contains? syms sym)
    decl
    (assoc ctx :form sym)))

(defn bind-arg [{{{sym :form :as decl} :form :as bind} :form :as im}]
  (let [did (get (:syms decl) sym)
        bids (get (:bindings bind) sym)]
    (if (contains? bids did)
      (get bids did)
      ;; If the binding doesn't apply to this declaration, toss it.
      (assoc im :form decl))))

;;;;; Contexts

(defprotocol ContextSwitch)

(defrecord Context [form ctx]
  Object
  (toString [_]
    (str "#C" (keys (names ctx)) "::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch)

(ast/ps Context)

(defmethod pp/simple-dispatch Context [{:keys [form ctx]}]
  (pp/write-out (str "#C" (keys (names ctx)) "," (decls ctx) "::"))
  (pp/simple-dispatch form))

(defrecord Declaration [form syms]
  Object
  (toString [_]
    (str "#D" syms "::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch)

(ast/ps Declaration)

(defmethod pp/simple-dispatch Declaration [{:keys [form syms]}]
  (pp/write-out (str "#D" syms "::"))
  (pp/simple-dispatch form))

(defrecord Binding [form bindings]
  Object
  (toString [_]
    (str "#B" bindings "::" form))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch)

(ast/ps Binding)

(defmethod pp/simple-dispatch Binding [{:keys [form bindings]}]
  (pp/write-out (str "#B" bindings "::"))
  (pp/simple-dispatch form))

(extend-protocol ast/Inspectable
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
  (->Declaration body (into {} (map (fn [x] [x id])) syms)))

(defn bind [body id bindings]
  (->Binding
   body
   (into {} (map (fn [[k v]] [k {id v}])) bindings)))

(def type-table
  {Context        :C
   Declaration    :D
   Binding        :B})

(defn merge-decls [{{form :form isyms :syms} :form osyms :syms}]
  (->Declaration form (merge isyms osyms)))

(defn merge-binds [{{form :form ibs :bindings} :form obs :bindings}]
  (->Binding form (merge-with merge ibs obs)))

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
      (ctx? inner)             ctx
      true                     inner)))
