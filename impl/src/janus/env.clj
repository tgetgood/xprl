(ns janus.env
  (:refer-clojure :exclude [declare resolve bound?])
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [janus.ast :as ast])
  (:import
   (java.io Writer)))

;;;;; Namespaces (contexts)

(def empty-ns
  {})

(defn ns-intern [ns sym val]
  (assoc ns sym val))

(defn ns-declare [ns sym]
  (dissoc ns sym))

(defn project
  "Fits `env` by removing all names not mentioned in `form`. "
  ([env form]
   (select-keys env (ast/symbols form))))

(defn lookup [env sym]
  (get env sym))

(defn bound? [env sym]
  (contains? env sym))

;;;;; Contexts

(defrecord ContextSwitch [form env]
  Object
  (toString [_]
    (str "#C" (keys env) "<" form ">"))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form)))

(ast/ps ContextSwitch)

(defmethod pp/simple-dispatch ContextSwitch [{:keys [form env]}]
  (pp/write-out (symbol "#C"))
  (pp/write-out (str (sort-by :names (keys env))))
  (pp/write-out  (symbol "<"))
  (pp/simple-dispatch form)
  (pp/write-out  (symbol ">")))

(extend-protocol ast/Inspectable
  ContextSwitch
  (insp [{:keys [form env]} ^Writer w level]
    (ast/spacer w level)
    (.write w "C")
    (.write w (str (sort-by :names (keys env))))
    (.write w "\n")
    (ast/insp form w (inc level))))

(defn ctx? [x]
  (instance? ContextSwitch x))

(defn prune [{:keys [form] :as ctx}]
  (update ctx :env project form))

(defn context-free? [form]
  (and (ctx? form) (= (:env form) empty-ns)))

(defn pin [body env]
  (->ContextSwitch body (project env body)))

(def type-table
  {ContextSwitch :C})
