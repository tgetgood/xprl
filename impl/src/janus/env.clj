(ns janus.env
  (:refer-clojure :exclude [declare resolve])
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [janus.ast :as ast])
  (:import
   (java.io Writer)))

;;;;; Namespaces
;;
;; TODO: Move this code somewhere else, it's confusing when mixed with the
;; interpreter's env. They're not quite the same.

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

;;;;; Contexts

(defprotocol ContextSwitch
  (merge-env [this env]))

(defrecord Context [form ctx]
  Object
  (toString [_]
    (str "#C" (keys (names ctx)) "<" form ">"))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (merge-env [_ _]
    ;; Override the environment with a new namespace.
    (names ctx)))

(ast/ps Context)

(defmethod pp/simple-dispatch Context [{:keys [form ctx]}]
  (pp/write-out (symbol "#C"))
  (pp/write-out (str (sort-by :names (keys (names ctx)))))
  (pp/write-out  (symbol "<"))
  (pp/simple-dispatch form)
  (pp/write-out  (symbol ">")))

(defrecord Declaration [form syms]
  Object
  (toString [_]
    (str "#D" (sort-by :names syms) "<" form ">"))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (merge-env [_ env]
    (reduce env dissoc syms)))

(ast/ps Declaration)

(defmethod pp/simple-dispatch Declaration [{:keys [form syms]}]
  (pp/write-out (symbol "#D"))
  (pp/write-out (str (sort-by :names syms)))
  (pp/write-out  (symbol "<"))
  (pp/simple-dispatch form)
  (pp/write-out  (symbol ">")))

(defrecord Binding [form bindings]
  Object
  (toString [_]
    (str "#B" (sort-by :names (keys bindings)) "<" form ">"))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form))
  ContextSwitch
  (merge-env [_ env]
    (merge env bindings)))

(ast/ps Binding)

(defmethod pp/simple-dispatch Binding [{:keys [form bindings]}]
  (pp/write-out (symbol "#B"))
  (pp/write-out (str (sort-by :names (keys bindings))))
  (pp/write-out  (symbol "<"))
  (pp/simple-dispatch form)
  (pp/write-out '>))

(extend-protocol ast/Inspectable
  Context
  (insp [{:keys [form ctx]} ^Writer w level]
    (ast/spacer w level)
    (.write w "C")
    (.write w (str (sort-by :names (keys (names ctx)))))
    (.write w "\n")
    (ast/insp form w (inc level)))

  Declaration
  (insp [{:keys [form syms]} ^Writer w level]
    (ast/spacer w level)
    (.write w "D")
    (.write w (str (sort-by :names syms)))
    (.write w "\n")
    (ast/insp form w (inc level)))

  Binding
  (insp [{:keys [form bindings]} ^Writer w level]
    (ast/spacer w level)
    (.write w "B")
    (.write w (str (sort-by :names (keys bindings))))
    (.write w "\n")
    (ast/insp form w (inc level))))

(defn pin [body env]
  (if (ast/contextual? body)
    (->Context body env)
    body))

(defn declare [body syms]
  (->Declaration body (into #{} syms)))

(defn bind [{inner :form syms :syms :as body} bindings]
  (println syms)
  (assert (every? #(contains? syms %) (keys bindings)) "Undeclared variable!")
  (->Binding inner bindings))

(def type-table
  {Context        :C
   Declaration    :C
   Binding        :C})

(defn context? [x]
  (instance? Context x))

(defn ctx? [x]
  (satisfies? ContextSwitch x))
