(ns janus.w1
  "Tree walking simplifier. Maybe backtracking is overkill."
  (:refer-clojure :exclude [reduce eval apply extend resolve run! reduced? delay?])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [janus.ast :as ast]
   [janus.reader :as r]))

(declare reduce-walk)

;;;;; env

(defn top-resolve [s]
  (println (-> s meta :lex (get s)))
  (if-let [b (-> s meta :lex (get s))]
    (with-meta (ast/symbol (str s) b) (meta s))
    (throw (RuntimeException. (str "unresolvable symbol: " s)))))

(defn resolve [s]
  (let [b (:binding s)]
    (println s b)
    (cond
      (= b :unbound) (top-resolve s)
      (symbol? b)    s
      true           (if (instance? clojure.lang.IMeta b)
                       (with-meta b (assoc (meta s) :reduced? true))
                       b))))

;;;;; application

(defn primitive-call [head tail]
  (println head)
  (println tail (meta tail))
  ((:f head) head tail))

(defn param-walk [id params args body]
  (let [f (fn [form]
            (if (and (ast/symbol? form) (= (:names params) (:names form)))
              (ast/symbol (str form) args)
              form))]
    (walk/prewalk f body)))

(defn param-set [{:keys [id params body]} args]
  (param-walk id params args body))

(defn μ-invoke [μ args]
  (let [args' (reduce-walk args)
        body' (param-set μ args')]
    (reduce-walk body')))

;;;;; walker

(def type-table
  {clojure.lang.PersistentVector :L
   janus.ast.Immediate           :I
   janus.ast.Pair                :P
   janus.ast.Symbol              :S
   janus.ast.Application         :A
   janus.ast.Primitive           :F
   janus.ast.Mu                  :μ})

(def eval-rules
  {;; (I (L x y ...)) => (L (I x) (I y) ...)
   :L (fn [xs] (let [ys (reduce-walk (into [] (map ast/immediate) xs))]
                 (with-meta ys {:reduced? (every? reduced? (map meta ys))})))
   ;; (I (P x y)) => (A (I x) y)
   :P (fn [{:keys [head tail]}] (reduce-walk
                                 (ast/application (ast/immediate head) tail)))
   :S (fn [s] (reduce-walk (resolve s)))})

(defn eval-walk [{:keys [form]}]
  (if-let [f (eval-rules (type-table (type form)))]
    (f form)
    form))

(def apply-rules
  {:μ μ-invoke
   :F primitive-call})

(defn apply-walk [{:keys [head tail] :as x}]
  (println head (type head))
  (let [h (reduce-walk head)]
    (if-let [f (apply-rules (type-table (type h)))]
      (f head tail)
      (with-meta (ast/application h tail) (meta x)))))

(def base-rules
  {:I (memoize eval-walk)
   :A (memoize apply-walk)})

(defn reduce-walk* [x]
  (if-let [f (base-rules (type-table (type x)))]
    (f x)
    x))

(def reduce-walk (memoize reduce-walk*))

(defn reduced? [x]
  (if (instance? clojure.lang.IMeta x)
    (:reduced? (meta x))
    true))

(defn reduce-tree [x]
  (if (reduced? x)
    x
    (let [o (reduce-walk x)]
      (if (identical? o x)
        x
        (reduce-tree o)))))

(defn simplify [x]
  (walk/postwalk reduce-tree x))

;;;;; Builtins

(defn createμ [_ [params body]]
  (println "!!!!!!!"))

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

(defn pwrap [f]
  (fn [p args]
    (if (reduced? args)
      (reduce-walk (f args))
      p)))

(defn tagged [f]
  (fn [[k v]]
    (let [k' (ast/symbol k)]
      [k' (f (with-meta v {:name k'}))])))

(defn tag-primitives [x]
  (into {} (map (tagged ast/->Primitive)) x))

(def base-env
  (merge
   (tag-primitives macros)
   (tag-primitives (into {} (map (fn [[k v]] [k (pwrap v)])) fns))))

;;;;; UI

(def srcpath "../src/")
(def bootxprl (str srcpath "boot.xprl"))

(def env (atom base-env))

(defn go! [f ccs]
  (simplify (ast/immediate f)))

(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @env))
       {}
       #_{(xkeys :return)  #(do (reset! out %) (println %))
            (xkeys :env)     #(swap! env assoc (first %) (second %))
            ;; (xkeys :unbound) (fn [x] (println "Unbound!" x))
            (xkeys :error)   (fn [e] (println {:msg   "top level error"
                                               :error e}))}))
