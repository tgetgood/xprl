(ns janus.env
  (:refer-clojure :exclude [declare resolve bound?])
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [janus.ast :as ast])
  (:import
   (java.io Writer)))

;;;;; Namespaces
;;
;; TODO: Move this code somewhere else, it's confusing when mixed with the
;; interpreter's env. They're not quite the same. But they're *mostly* the same.

(def empty-ns
  {:names {} :declarations #{}})

(defn ns-intern [ns sym val]
  (-> ns
      (update :names assoc sym val)
      (update :declarations disj sym)))

(defn ns-declare [ns sym]
  (-> ns
      (update :names dissoc sym)
      (update :declarations conj sym)))

(defn project
  "Fits `env` by removing all names not mentioned in `form`. Keeps
  declarations."
  [env form]
  (let [syms (ast/symbols form)]
    (-> env
        (update :declarations set/intersection syms)
        (update :names select-keys syms))))

(defn lookup [env sym]
  (get-in env [:names sym]))

(defn bound? [env sym]
  (contains? (:names env) sym))

(defn declared? [env sym]
  (contains? (:declarations env) sym))

;;;;; Contexts

(defrecord Context [form env]
  Object
  (toString [_]
    (str "#C" #_(keys bindings) "<" form ">"))
  janus.ast.Contextual
  janus.ast.Symbolic
  (symbols [_]
    (ast/symbols form)))

(ast/ps Context)

(defmethod pp/simple-dispatch Context [{:keys [form]}]
  (pp/write-out (symbol "#C"))
  #_(pp/write-out (str (sort-by :names (keys bindings))))
  (pp/write-out  (symbol "<"))
  (pp/simple-dispatch form)
  (pp/write-out  (symbol ">")))

(extend-protocol ast/Inspectable
  Context
  (insp [{:keys [form]} ^Writer w level]
    (ast/spacer w level)
    (.write w "C")
    #_(.write w (str (sort-by :names (keys bindings))))
    (.write w "\n")
    (ast/insp form w (inc level))))

(defn pin [body env]
  (->Context body (project env body)))

(defn bind [env bindings]
  (reduce (fn [env [k v]] (ns-intern env k v)) env bindings))

(defn declare [env syms]
  ;; REVIEW: This nil? check is ~probably~ unnecessary
  (transduce (remove nil?) (completing ns-declare) env syms))

(defn fill-slots [env dyn]
  (transduce (filter #(bound? dyn %))
             (completing (fn [env k] (ns-intern env k (lookup dyn k))))
             (or env empty-ns)
             (:declarations env)))

(def type-table
  {Context :C})

(defn ctx? [x]
  (instance? Context x))

(defn context-free? [form]
  (and (ctx? form) (= (:env form) empty-ns)))

(defn resolve [env sym]
  ;; REVIEW: This check is ~probably~ unnecessary, but I'm leaving it for now.
  (if (and (bound? env sym) (not (declared? env sym)))
    (lookup env sym)
    ::unresolved))
