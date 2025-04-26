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

(defn reduced? [x]
  (if (instance? clojure.lang.IMeta x)
    (:reduced? (meta x))
    true))

(defn reduced [x]
  (if (instance? clojure.lang.IMeta x)
    (with-meta x (assoc (meta x) :reduced? true))
    x))

(defn primitive-call [head tail]
  (let [args (reduce-walk tail)]
    (if (reduced? args)
      (clojure.core/apply (:f head) args)
      (ast/application head args))))

(defn macro-call [head tail]
  ((:f head) tail))

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
    body'))

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

(declare eval-walk)

(def eval-rules
  {;; (I (L x y ...)) => (L (I x) (I y) ...)
   :L (fn [xs] (into [] (map ast/immediate) xs))
   ;; (I (P x y)) => (A (I x) y)
   :P (fn [{:keys [head tail]}] (ast/application (ast/immediate head) tail))
   :I (fn [x] (ast/immediate (eval-walk x)))
   :S (fn [s] (resolve s))})

(defn eval-walk [{:keys [form]}]
  (trace! "ewalk" (type form) form)
  (if-let [f (eval-rules (type-table (type form)))]
    (let [v (f form)]
      (trace! "ewalkm" (type form) form v (= v form))
      (if (= v form)
        (ast/immediate v)
        v))
    form))

(def apply-rules
  {:μ μ-invoke
   :F primitive-call
   :M macro-call})

(defn apply-walk [{:keys [head tail] :as x}]
  (trace! "awalk" (type head) x)
  (let [h (reduce-walk head)]
    (if-let [f (apply-rules (type-table (type h)))]
      (f h tail)
      (with-meta (ast/application h tail) (meta x)))))

(def reduce-rules
  {:I (memoize eval-walk)
   :A (memoize apply-walk)
   :μ (fn [{:keys [id name params body]}]
        (let [p'    (reduce-walk params)
              body' (param-walk id p' id body)]
          (ast/μ id name p' (reduce-walk body'))))
   :L (fn [xs]
        (let [ys (into [] (map reduce-walk) xs)]
          (with-meta ys {:reduced? (every? reduced? ys)})))})

(defn reduce-walk* [x]
  (trace! "rwalk" (type x) x)
  (if (reduced? x)
    x
    (if-let [f (reduce-rules (type-table (type x)))]
      (let [v (f x)]
        (if (= x v)
          v
          (reduce-walk (f x))))
      x)))

(def reduce-walk (memoize reduce-walk*))

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

(defn go! [f ccs]
  (reduce-walk (ast/immediate f)))

(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @env))
       {}
       #_{(xkeys :return)  #(do (reset! out %) (println %))
            (xkeys :env)     #(swap! env assoc (first %) (second %))
            ;; (xkeys :unbound) (fn [x] (println "Unbound!" x))
            (xkeys :error)   (fn [e] (println {:msg   "top level error"
                                               :error e}))}))
