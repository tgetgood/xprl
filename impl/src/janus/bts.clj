(ns janus.bts
  "Back tracking tree simplifier. Name says it all if it says anything."
  (:refer-clojure :exclude [eval apply reduce reduced? delay])
  (:require
   [janus.ast :as ast]
   [janus.runtime :as rt]))

(defprotocol Reduce
  (reduce* [x dyn]))

(defn reduce [f dyn]
  (reduce* f dyn))

(defprotocol Eval
  (eval* [x dyn]))

(defn eval [x dyn]
  (eval* x dyn))

(defprotocol Apply
  (apply* [head tail dyn]))

(defn apply [head tail dyn]
  (apply* head tail dyn))

;; Extend for Clojure types

(defmacro extend-colls [types]
  `(do
     ~@(into []
             (map
              (fn [t#]
                `(extend ~t#
                   Reduce
                   {:reduce* (fn [xs# dyn#] (into (empty xs#) (map #(reduce % dyn#)) xs#))}
                   Eval ; (I (L x y ...)) => (L (I x) (I y) ...)
                   {:eval* (fn [xs# dyn#] (into (empty xs#) (map #(eval % dyn#)) xs#))})))
             types)))

(extend-colls
 [clojure.lang.APersistentMap
  clojure.lang.APersistentSet
  clojure.lang.APersistentVector])

(extend clojure.lang.AMapEntry
  Reduce
  {:reduce* (fn [e dyn]
              (clojure.lang.MapEntry. (reduce (key e) dyn) (reduce (val e) dyn))) }
  Eval
  {:eval* (fn [e]
            (clojure.lang.MapEntry. (eval (key e)) (eval (val e))))})

;; xprl logic

(extend-protocol Reduce
  Object
  (reduce* [x] x)

  janus.ast.Immediate
  (reduce* [{:keys [form]}]
    (eval form))

  janus.ast.Application
  (reduce* [{:keys [head tail]}]
    (apply (reduce head) tail)))

(extend-protocol Eval
  Object
  (eval* [x] x)

  janus.ast.Symbol
  (eval* [s]
    (if-let [v (get *dyn* s)] ; s is a μ parameter
      (reduce v)
      (if-let [v (-> s meta :lex (get s))] ; s is lexical
        (reduce v)
        (throw (RuntimeException. "unbound variable.")))))

  janus.ast.Pair
  (eval* [{:keys [head tail]}] ; (I (P x y)) => (A (I x) y)
    (apply (eval head) tail))

  janus.ast.Immediate
  (eval* [{:keys [form]}]
    (eval (eval form)))

  janus.ast.Application
  (eval* [x] ; (I (A x y)) must be treated in applicative order.
    (eval (reduce x))))

(extend-protocol Apply
  janus.ast.Application
  (apply* [head tail]
    (apply (reduce head) tail))

  janus.ast.PrimitiveMacro
  (apply* [mac tail]
    ((:f mac) tail))

  janus.ast.PrimitiveFunction
  (apply* [f args]
    (clojure.core/apply (:f f) (reduce args)))

  janus.ast.Mu
  ;; REVIEW: Should we allow user defined types to extend apply?
  ;; Meta metacircularity protocol?
  ;; I don't think so, but it's a cute idea.
  (apply* [μ args]
    (let [bind (ast/destructure (:params μ) (reduce args))
          Δenv (if (= ::anon (:name μ)) bind (assoc bind (:name μ) μ))]
      (with-bindings {(var *dyn*) (merge *dyn* Δenv)}
        (reduce (:body μ))))))

(defn createμ [args]
  (cond
    (= 2 (count args)) (createμ [::anon (first args) (second args)])
    (= 3 (count args)) (let [[name params body] args]
                         (ast/μ name (reduce params) body))))
