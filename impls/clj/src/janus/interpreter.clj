(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:import [janus.ast Immediate])
  (:require [janus.ast :as ast]
            [janus.runtime :as rt]))

(defn ni [] (throw (RuntimeException. "not implemented")))
(def return (ast/keyword "return"))

(def unboundmarker
  "Value used by μs to indicate an env var is to be bound. Needed to shadow
  lexical env with params."
  (gensym))

(defn succeed [c & v]
  (rt/emit c (ast/keyword "return") v))

(defn createμ [[params body] env c])

(defprotocol Reductive
  (reduced? [this])
  (reduce [this env c] {:style/indent 2}))

(defprotocol Evaluable
  (eval [this env c]))

(defprotocol Applicable
   (apply [this args env c]))

(extend-protocol Reductive
  Object
  (reduced? [_] true)
  (reduce [o env c] (succeed c o))

  clojure.lang.APersistentVector
  (reduced? [x] (every? reduced? x))
  (reduce [this env c]
    (let [collector (rt/collector c (count this))
          runner (fn [i x]
                   (reduce
                    x
                    env
                    (rt/withcc c {return (fn [v] (rt/receive collector i v))})))]
      (clojure.core/apply rt/emit c (interleave (repeat runner)
                                             (map-indexed vector this)))))

  clojure.lang.AMapEntry
  (reduced? [x] (and (reduced? (key x)) (reduced? (val x))))
  (reduce [this env c]
    (ni))

  clojure.lang.APersistentMap
  (reduced? [x] (every? reduced? x))
  (reduce [this env c]
    (ni))

  clojure.lang.APersistentSet
  (reduced? [x] (every? reduced? x))
  (reduce [this env c]
    (ni))

  janus.ast.Immediate
  (reduced? [_] false)
  (reduce [x env c]
    (eval (:form x) env c))

  janus.ast.Application
  (reduced? [_] false)
  (reduce [x env c]
    (reduce (:head x) env (rt/withcc c {return #(apply % (:tail x) env c)})))

  janus.ast.PartialMu
  (reduced? [_] false)
  (reduce [x env c]
    (createμ [(:params x) (:body x)] env c))

  janus.ast.Mu
  (reduced? [x] (reduced? (:body x)))
  (reduce [x env c]
    (createμ [(:params x) (:body x)] env c))

  janus.ast.Pair
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x))))
  (reduce [x env c]
    (succeed c x)))

(extend-protocol Evaluable
  Object
  (eval [o env c]
    (succeed c o))

  clojure.lang.APersistentVector
  (eval [this env c]
    (reduce (mapv ast/->Immediate this) env c))

  janus.ast.Symbol
  (eval [this env c]
    (println this)
    (if-let [v (get env this)]
      (if (= v unboundmarker)
        (ast/->Immediate this)
        (reduce v env c))
      ;; What carries its meaning on its back?
      (if-let [v (-> this meta :env (get this))]
        (reduce v env c)
        (println (str "ERROR: unbound symbol: " this)))))

  janus.ast.Pair
  (eval [{:keys [head tail]} env c]
    (reduce (ast/->Application (ast/->Immediate head) tail) env c))

  janus.ast.Immediate
  (eval [{:keys [form]} env c]
    (letfn [(next [form]
              (if (instance? janus.ast.Immediate form)
                (succeed c (ast/->Immediate form))
                (eval form env c)))]
      (reduce form env (rt/withcc c return next))))

  janus.ast.Application
  (eval [form env c]
    (letfn [(next [form]
              (if (instance? janus.ast.Application form)
                (succeed c (ast/->Immediate form))
                (eval form env c)))]
      (reduce form env (rt/withcc c return next))))


  )

(extend-protocol Applicable
  janus.ast.Immediate
  ;; Immediates cannot be applied. This is a delay tactic.
  (apply [head tail env c]
    (succeed c (ast/->Application head tail)))

  janus.ast.Application
  (apply [head tail env c]
    (letfn [(next [head]
              (if (instance? janus.ast.Application head)
                (succeed c (ast/->Application head tail))
                (apply head tail env c)))]
      (reduce head env (rt/withcc c {return next}))))

  janus.ast.Mu
  (apply [head tail env c] (ni))

  janus.ast.PartialMu
  (apply [head tail env c] (ni))

  janus.ast.PrimitiveMacro
  (apply [head tail env c]
    ((:f head) tail env c))

  janus.ast.PrimitiveFunction
  (apply [head tail env c]
    (letfn [(next [tail]
              (succeed c (if (reduced? tail)
                           (clojure.core/apply (:f head) tail)
                           (ast/->Application head tail))))]
      (reduce tail env (rt/withcc c return next)))))
