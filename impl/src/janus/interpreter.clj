(ns janus.interpreter
  (:refer-clojure :exclude [resolve run! binding declare])
  (:import (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [clojure.walk :as walk]
   [janus.ast :as ast]
   [janus.reader :as r]))

(def verbose true)

(defmacro trace! [& args]
  (when verbose
    `(println ~@args)))

;;;;; Environment

(def local 'ns-local)

(defn local? [x]
  (symbol? x))

(defn resolve [env s]
  (if-let [ref (get env s)]
    (if (local? ref)
      (ast/immediate s)
      ref)
    ;; HACK: This is intended to prevent μ bodies that expand to include symbols that were expanded to create them from
    (if-let [ref (get env [:inner s])]
      (ast/immediate s)
      (throw (RuntimeException. (str "Unbound symbol: " s))))))

(defn declare
  ([env s] (declare env s local))
  ([env s id] (assoc env s id)))

(defn μ-declare [env μ]
  (let [e' (declare env (:params μ) (:id μ))]
    (if (:name μ)
      (declare e' (:name μ) (:id μ)))))

(defn bind [env s val id]
  (let [ref (get env s)]
    (assert (and ref (= ref id)))
    (assoc env s val)))

(defn μ-bind [env μ args]
  (let [env' (bind env (:params μ) args (:id μ))]
    (if (:name μ)
      (bind env' (:name μ) μ (:id μ))
      env')))

;;;;; application

(clojure.core/declare reduce-walk)

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

(defn primitive-reduced? [x]
  (and (vector? x) (every? evaluated? x)))

(defn primitive-call [env [head tail]]
  (let [args (reduce-walk env tail)]
    (trace! "pcall" head args)
    (if (primitive-reduced? args)
      (clojure.core/apply (:f head) args)
      (ast/application head args))))

(defn macro-call [env [head tail]]
  ((:f head) env tail))

(defn μ-invoke [env [μ args]]
  (trace! "invoke" μ "with" args)
  (let [args' (reduce-walk env args)]
    (if (and (ast/symbol? (:params μ)) (evaluated? args'))
      (reduce-walk (μ-bind env μ args) (:body μ))
      (ast/application μ args'))))

(defn application [_ [head tail]]
  (ast/application head tail))

;;;;; reduction

(defn μ-reduce [env μ]
  (if (ast/symbol? params)
    (assoc μ :body (reduce-walk (μ-declare env μ) (:body μ)))
    ;; When the params and body are both computed at the same time, the code
    ;; that generates the body cannot refer to the param that will be generated
    ;; because it is not yet defined.
    ;;
    ;; FIXME: That means that the reduction needs to switch environments after
    ;; the body has been computed, but before it gets reduced.
    ;;
    ;; How on earth do I accomplish that?
    (let [p' (reduce-walk env (:params μ))
          e' (if (ast/symbol? p') (declare env [:inner p'] (:id μ)) env)]
      (assoc μ
             :params p'
             :body (reduce-walk e' (:body μ))))))

(defn emit-walk [acc kvs]
  (if (seq kvs)
    (let [[k v] (first kvs)
          k' (reduce-walk (ast/immediate k))
          v' (reduce-walk v)]
      (trace! "emit-walk" k "->" k' ":" v "->" v')
      (recur (conj acc [k' v']) (rest kvs)))
    acc))

(defn reduce-emission [{:keys [kvs] :as e}]
  (assoc e :kvs (emit-walk [] kvs)))

;;;;; Interpreter

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

(defmacro defwalker [name rules params k arg found not-found]
  `(defn ~name [env# ~@params]
     (let [t# (ast-type ~k)]
       (trace! (name '~name) "rule" t# ~arg "with" env#)
       (if-let [f# (~rules t#)]
         (let [v# (f# env# ~arg)]
           (trace! (name '~name) "post" (ast-type v#) v#)
           (if (env-tagged? v#)
             (~found (:env v#) (:form v#))
             (~found env# v#)))
         ~not-found))))

(def eval-rules
  {;; (I (L x y ...)) => (L (I x) (I y) ...)
   :L (fn [xs] (into [] (map ast/immediate) xs))
   ;; (I (P x y)) => (A (I x) y)
   :P (fn [{:keys [head tail]}] (ast/application (ast/immediate head) tail))
   :S resolve})

(defwalker eval-walk eval-rules [x] x x
  (fn [_ x] x) ; drop the env
  ;; Put the `I` back if you can't evaluate.
  (if (evaluated? x) x (ast/immediate x)))

(def apply-rules
  {:μ μ-invoke
   :F primitive-call
   :M macro-call
   ;; These (E, I & A) are here as rules since fallthrough allows anything to be
   ;; in the head of a pair without triggering an error (the computation just
   ;; stalls on an A node.
   :E application
   :I application
   :A application})

(defwalker apply-walk apply-rules [head tail] head [head tail]
  (fn [_ x] x)
  (throw (RuntimeException. (str "cannot apply (" (type head) ") " head ))))

(def reduce-rules
  {:I (fn [{:keys [form]}] (eval-walk (reduce-walk form)))
   :A (fn [{:keys [head tail]}] (apply-walk (reduce-walk head) tail))
   :μ μ-reduce
   :L (fn [xs] (into [] (map reduce-walk) xs))
   :E reduce-emission})

(defwalker reduce-walk reduce-rules [x] x x
  (fn [env val] (if (= x val) val (reduce-walk env val)))
  x)

;;;;; Builtins

(defn createμ [env args]
  (let [[name params body] (if (= 3 (count args)) args `[nil ~@args])]
    (with-meta (ast/μ (gensym) name params body env)
      (meta body))))

(defn emit [_ kvs]
  (assert (even? (count kvs)))
  (with-meta (ast/emission (map vec (partition 2 kvs)))
    (meta kvs)))

(defn select {:name "select"} [_ [p t f]]
  (let [p (reduce-walk p)]
    (cond
      (boolean? p)         (if p t f)
      (not (evaluated? p)) (ast/application (ast/->Macro #'select) [p t f]))))
