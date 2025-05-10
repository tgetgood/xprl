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

(defn nearest-env [x k]
  (let [v     (get x k)
        outer (or (get-env x) (empty-ns))
        inner (or (get-env v) outer)
        e     (bind-decls inner outer)]
    (set-env v e)))

;;;;; Accessors

(defn name [x] (nearest-env x :name))
(defn params [x] (nearest-env x :params))
(defn body [x] (nearest-env x :body))
(defn head [x] (nearest-env x :head))
(defn tail [x] (nearest-env x :tail))
(defn form [x] (nearest-env x :form))

;;;;; Application

(defn μ-bind [μ args]
  (let [body (body μ)
        env (bind (get-env body) (params μ) args)
        env (if (name μ) (bind env (name μ) μ) env)]
    (set-env body env)))

(defn μ-call [env [μ args]]
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

(defn primitive-call [[head tail]]
  (let [args (walk tail)]
    (trace! "pcall" head args)
    (if (primitive-reduced? args)
      (apply (:f head) args)
      (ast/application head args))))

(defn macro-call [env [head tail]]
  ((:f head) env tail))

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

(defn emit-reduce [x])

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
  {:down {[:I :S] resolve   ; This is where the env is read

          [:I :P] (fn [p] ; (I (P x y)) => (A (I x) y)
                    (walk (ast/application (ast/immediate (head p)) (tail p))))

          [:I :L] (fn [xs] ; (I (L x y ...)) => (L (I x) (I y) ...)
                    (walk (into [] (map ast/immediate xs))))

          [:I :V] identity ; (I V) => V. values eval to themselves

          ;; In general we cannot walk into structures. These are the exceptions.
          :L (fn [xs] (into [] (map walk) xs))
          :μ μ-reduce
          :E emit-reduce

          ;; Three kinds of applicables are built in. I don't think we need any
          ;; others, but that might change.
          [:A :M] macro-call
          [:A :F] primitive-call
          ;; REVIEW: Does it matter if the tail is currently reducible?
          ;; So long and the name and params of the μ are reduced to symbols, we
          ;; can apply any args in a purely syntactic fashion.
          ;; Whether that's useful is to be determined.
          [:A :μ] μ-call}

   ;; The upwards pass reconstructs the AST from whatever couldn't be evaluated
   ;; on the way down.
   ;; REVIEW: Does anything here look suspicious to you?
   :up {[:I :S] identity
        [:I :A] identity
        [:I :I] identity
        [:A :E] identity ; this one's a little fishy...
        [:A :I] identity
        [:A :A] identity
        [:A :F] identity

        ;; The application of a primitive macro cannot be postponed, so we don't
        ;; have a case for [:A :M]
        :A      'error}})
