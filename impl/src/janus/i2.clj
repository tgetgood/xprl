(ns janus.i2
  (:refer-clojure :exclude [resolve run! binding name])
  (:import (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [janus.ast :as ast]
   [janus.reader :as r]))


(def verbose true)

(defmacro trace! [& args]
  (when verbose
    `(println ~@args)))

(clojure.core/declare walk)

;;;;; Environment

(defn empty-ns []
  ;; REVIEW: One sanity check on a namespace is that the set of declared (but
  ;; not instantiated) names must be empty by the time we finish reading it in.
  {:names {} :declared #{}})

(defn lookup [env sym]
  (get-in env [:names sym]))

(defn decls [env]
  (:declared env))

(defn get-env [x]
  (when (instance? clojure.lang.IMeta x)
    (::env (meta x))))

(defn set-env [x env]
  (if (instance? clojure.lang.IMeta x)
    (with-meta x (assoc (meta x) ::env env))
    x))

(defn resolve [s]
  (let [env (get-env s)]
    (if-let [ref (lookup env s)]
      (do
        (trace! "resolve" s ":" ref)
        (walk ref))
      (if (contains? (decls env) s)
        (do
          (trace! "declared" s)
          (ast/immediate s))
        (throw (RuntimeException. (str "Unbound symbol: " s)))))))

(defn bind [env s val]
  (-> env
      (assoc-in [:names s] val)
      (update :declared disj s)))

(defn μ-declare-1 [env s]
  (-> env
      (update :declared conj s)
      (update :names dissoc s)))

(defn bind-decls [inner outer]
  (reduce (fn [e sym] (if-let [val (lookup outer sym)] (bind e sym val) e))
          inner
          (decls inner)))

(defn merge-env [x y]
  (let [outer (or (get-env x) (empty-ns))
        inner (or (get-env y) outer)]
    (bind-decls inner outer)))

(defn nearest-env [x k]
  (let [v (get x k)
        e (merge-env x v)]
    (set-env v e)))

;;;;; Accessors

(defn params [x] (nearest-env x :params))
(defn name   [x] (nearest-env x :name))
(defn body   [x] (nearest-env x :body))
(defn head   [x] (nearest-env x :head))
(defn tail   [x] (nearest-env x :tail))
(defn form   [x] (nearest-env x :form))
(defn kvs    [x] (nearest-env x :kvs))

;;;;; Application

(defn μ-bind [μ args]
  (let [body (body μ)
        env (bind (get-env body) (params μ) args)
        env (if (name μ) (bind env (name μ) μ) env)]
    (set-env body env)))

(defn μ-call [μ args]
  (trace! "invoke" μ "with" args)
  (if (and (ast/symbol? (params μ)) (or (nil? (name μ)) (ast/symbol? (name μ))))
    (walk (μ-bind μ args))
    (ast/application μ args)))

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

(defn primitive-reduced? [x]
  (and (vector? x) (every? evaluated? x)))

(defn primitive-call [head tail]
  (let [args (walk tail)]
    (trace! "pcall" head args)
    (if (primitive-reduced? args)
      (apply (:f head) args)
      (ast/application head args))))

(defn macro-call [head tail]
  ;; Macros can't be applied if they don't even have syntactic args.
  (let [args (if (vector? tail) tail (walk tail))]
    (if (vector? args)
      ((:f head) args)
      (ast/application head args))))

;;;;; Reduction

(defn μ-declare [μ]
  (let [body (body μ)
        env (μ-declare-1 (get-env body) (params μ))
        env (if (name μ) (μ-declare-1 env (name μ)) env)]
    (set-env body env)))

(defn partial? [μ]
  (not (and (or (nil? (name μ)) (ast/symbol? (name μ))) (ast/symbol? (params μ)))))

(defn μ-reduce [μ]
  (if (partial? μ)
    (let [μ' (ast/μ (walk (name μ)) (walk (params μ)) (walk (body μ)))]
      (if (partial? μ')
        μ'
        (walk μ')))
    (assoc μ :body (walk (μ-declare μ)))))

(defn emit-reduce [x]
  (ast/emission (walk (kvs x))))

(defn list-reduce [xs]
  (with-meta (into [] (map #(walk (set-env % (merge-env % xs)))) xs) (meta xs)))

;;;;; Eval

(defn eval-list [xs]
  (walk (with-meta (into [] (map ast/immediate xs)) (meta xs))))

(defn eval-pair [p]
  (walk (with-meta (ast/application (ast/immediate (head p)) (tail p)) (meta p))))

(defn eval-eval [x]
  (let [inner (walk x)]
    (if (evaluated? inner)
      (walk (ast/immediate inner))
      (ast/immediate inner))))

;;;;; Tree walker

(def type-table
  {clojure.lang.PersistentVector :L
   janus.ast.Immediate           :I
   janus.ast.Pair                :P
   janus.ast.Symbol              :S
   janus.ast.Application         :A
   janus.ast.Primitive           :F
   janus.ast.Macro               :M
   janus.ast.Mu                  :μ
   janus.ast.Emission            :E})

(defn ast-type [x]
  (get type-table (type x) :V))

(def rules
  {[:I :S] resolve   ; env lookup
   [:I :P] eval-pair ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-eval ;
   [:I :V] identity  ; (I V) => V. values eval to themselves

   ;; In general we cannot walk into structures. These are the exceptions.
   :L list-reduce
   :μ μ-reduce
   :E emit-reduce

   ;; Three kinds of operators are built in. I don't think we need any others,
   ;; but that might change.
   ;;
   ;; In fact, I'm not 100% convinced we need to distinguish primitive fns from
   ;; primitive macros as a language feature. It ought to be possible to
   ;; implement a primitive fn as a primitive macro, but it has proven mightily
   ;; inconvenient.
   [:A :M] macro-call
   [:A :F] primitive-call
   [:A :μ] μ-call})

(defn make-tree [rules]
  (reduce (fn [acc [k v]]
            ;; FIXME: This works for now because there are no conflicting prefixes.
            (assoc-in acc (if (vector? k) k [k]) v))
          {} rules))

(defn step [x]
  (cond
    (instance? janus.ast.Immediate x)   (form x)
    (instance? janus.ast.Application x) (head x)
    true                                nil))

(defn walk1 [rules sexp]
  (when sexp
    (let [t (ast-type sexp)]
      (when-let [inst (get rules t)]
        (if (fn? inst)
          [inst sexp]
          (recur inst (step sexp)))))))

(defn smart-call [f x]
  (cond
    (instance? janus.ast.Immediate x)   (f (form x))
    (instance? janus.ast.Application x) (f (head x) (tail x))
    true                                (f x)))

(defn walk [sexp]
  (if-let [f (walk1 (make-tree rules))]
    (smart-call f sexp)
    sexp))

;;;;; Builtins

(defn createμ [args]
  (let [[name params body] (if (= 3 (count args)) args `[nil ~@args])]
    (walk (ast/μ name params body))))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (walk (ast/emission
         (map (fn [[k v]] [(ast/immediate k) v]) (partition 2 kvs)))))

(defn select {:name "select"} [[p t f]]
  (let [p (if (boolean? p) p (walk p))]
    (cond
      (boolean? p)         (walk (if p t f))
      (not (evaluated? p)) (ast/application (ast/macro #'select) [p t f]))))
