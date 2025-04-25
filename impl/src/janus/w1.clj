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

;;;;; env

(defn resolve [s]
  (let [b (:binding s)]
    (cond
      (= b :unbound) (throw (RuntimeException. (str "unbound symbol: " s)))
      (symbol? b)    s
      b)))

;;;;; walker

(declare descend)

(def type-table
  {:L clojure.lang.PersistentVector
   :I janus.ast.Immediate
   :P janus.ast.Pair
   :S janus.ast.Symbol
   :A janus.ast.Application})

(def eval-rules
  {;; (I (L x y ...)) => (L (I x) (I y) ...)
   :L (fn [xs] (into [] (map ast/immediate) xs))
   ;; (I (P x y)) => (A (I x) y)
   :P (fn [{:keys [head tail]}] (ast/application (ast/immediate head) tail))
   :S (fn [s] (resolve s))})

(def apply-rules
  {})

(def base-rules
  {:I (fn [x] (eval-rules (:form x)))
   :A (fn [{:keys [head tail]}]
         (let [h (descend head)]
           ((apply-rules v) tail)))})

(defn walker [x]
  (let [f (get base-rules (type x) descend)
        v (f x)]
    (if (identical? v x)
      v
      (walker x))))
