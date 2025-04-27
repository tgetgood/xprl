(ns janus.w1
  "Tree walking simplifier. Maybe backtracking is overkill."
  (:refer-clojure :exclude [reduce eval apply extend resolve run! reduced? reduced])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [janus.ast :as ast]
   [janus.reader :as r]))

(def verbose true)

(defmacro trace! [& args]
  (when verbose
    `(println ~@args)))

(declare reduce-walk)

;;;;; env

(defn top-resolve [s]
  (trace! "static resolve" s (-> s meta :lex (get s)))
  (if-let [b (-> s meta :lex (get s))]
    (with-meta b (meta s))
    (throw (RuntimeException. (str "unresolvable symbol: " s)))))

(defn resolve [s]
  (let [b (:binding s)]
    (trace! "resolve" s b)
    (cond
      (= b :unbound) (top-resolve s)
      (symbol? b)    s
      true           b)))

;;;;; application

(defn ast-indeterminate? [x]
  (some #(instance? % x)
        [janus.ast.Immediate janus.ast.Application #_janus.ast.Symbol]))

(defn evalable? [x]
  (cond
    (instance? janus.ast.Symbol x)    (not (symbol? (:binding x)))
    (instance? janus.ast.Immediate x) (evalable? (:form x))
    true                              true))

(defn reduced? [x]
  (if (vector? x)
    (every? reduced? x)
    (not (ast-indeterminate? x))))

(defn primitive-call [[head tail]]
  (let [args (reduce-walk tail)]
    (trace! "pcall" head args)
    (if (reduced? args)
      (clojure.core/apply (:f head) args)
      (ast/application head args))))

(defn macro-call [[head tail]]
  ((:f head) tail))

(defn param-walk [id params args body]
  (let [f (fn [form]
            (if (and (ast/symbol? form) (= (:names params) (:names form)))
              (ast/symbol (str form) args)
              form))]
    (walk/postwalk f body)))

(defn param-set [{:keys [id params body]} args]
  (param-walk id params args body))

(defn clean-param [p]
  (if (ast/symbol? p)
    (ast/symbol (str p) :μ-param)
    p))

(defn μ-invoke [[μ args]]
  (let [args' (reduce-walk args)
        body' (param-set μ args')]
    body'))

(defn μ-reduce [{:keys [name id params body]}]
  (let [p'     (clean-param (reduce-walk params))
        body'  (if (ast/symbol? p') (param-walk id p' id body) body)
        body'' (reduce-walk body')]
    (ast/μ id name p' body'')))

;;;;; walker

(def type-table
  {clojure.lang.PersistentVector :L
   janus.ast.Immediate           :I
   janus.ast.Pair                :P
   janus.ast.Symbol              :S
   janus.ast.Application         :A
   janus.ast.Primitive           :F
   janus.ast.Macro               :M
   janus.ast.Mu                  :μ})

;; REVIEW: This is overengineering looked at simply. But the point is to force
;; out the similarity in the three "types" of walkers and hopefully unify them.
(defmacro defwalker [name rules params k arg found not-found]
  `(defn ~name ~params
     (let [t# (type ~k)
           tk# (type-table t#)]
       (trace! (name '~name) "rule:" tk# "arg:" ~arg)
       (if-let [f# (~rules tk#)]
         (let [v# (f# ~arg)]
           (trace! (name '~name) "step" (type v#) v#)
           (~found v#))
         ~not-found))))

(declare eval-walk)

(def eval-rules
  {;; (I (L x y ...)) => (L (I x) (I y) ...)
   :L (fn [xs] (into [] (map ast/immediate) xs))
   ;; (I (P x y)) => (A (I x) y)
   :P (fn [{:keys [head tail]}] (ast/application (ast/immediate head) tail))
   :I (fn [x] (eval-walk x))
   :S resolve})

(defwalker eval-walk eval-rules [{:keys [form]}] form form
  #(if (evalable? %) % (ast/immediate %))
  form)

(def apply-rules
  {:μ μ-invoke
   :F primitive-call
   :M macro-call})

(defwalker apply-walk apply-rules [head tail] head [head tail]
  identity
  (ast/application head tail))

(def reduce-rules
  {:I eval-walk  ; TODO: memoise
   :A (fn [{:keys [head tail]}] (apply-walk (reduce-walk head) tail)) ; TODO: memoise
   :μ μ-reduce
   :L (fn [xs] (into [] (map reduce-walk) xs))})

(defwalker reduce-walk reduce-rules [x] x x
  #(if (= x %) % (reduce-walk %))
  x)

;;;;; Builtins

(defn createμ [[params body]]
  (ast/μ (gensym) "" params body))

(def macros
  {"μ"      createμ
   ;;   "ν"       createν
   ;; "emit"   emit
   ;;   "select"  select
   ;; "first*" first*
   ;; "rest*"  rest*
   })

(defn nth* [c i]
  (nth c (dec i)))

(def fns
  {"+*"   +
   "**"   *
   "-*"   -
   "/*"   /
   ">*"   >
   "<*"   <
   "=*"   =
   "mod*" mod

   "count*" count
   "nth*"   nth* ; Base 1 indexing
   })

(defn tagged [f]
  (fn [[k v]]
    (let [k' (ast/symbol k)]
      [k' (f (with-meta v {:name k'}))])))

(defn tag-primitives [x f]
  (into {} (map (tagged f)) x))

(def base-env
  (merge
   (tag-primitives macros ast/->Macro)
   (tag-primitives fns ast/->Primitive)))

;;;;; UI

(def srcpath "../src/")
(def bootxprl (str srcpath "boot.xprl"))

(def env (atom base-env))

(defn go! [f]
  (reduce-walk (ast/immediate f)))

(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @env))))
