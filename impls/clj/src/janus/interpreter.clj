(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:require [janus.ast :as ast]
            [janus.runtime :as rt]))

(defn ni [] (throw (RuntimeException. "not implemented")))

(def unboundmarker (gensym))

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
                    (rt/withcc c {:return (fn [v] (rt/receive collector i v))})))]
      (clojure.core/apply rt/emit c (interleave (repeat runner)
                                             (map-indexed vector this)))))

  clojure.lang.AMapEntry
  (reduced? [x] (and (reduced? (key x)) (reduced? (val x))))

  clojure.lang.APersistentMap
  (reduced? [x] (every? reduced? x))

  clojure.lang.APersistentSet
  (reduced? [x] (every? reduced? x))

  janus.ast.Immediate
  (reduced? [_] false)
  (reduce [x env c]
    (eval (:form x) env c))

  janus.ast.Application
  (reduced? [_] false)
  (reduce [x env c]
    (reduce (:head x) env (rt/withcc c {:return #(apply % (:tail x) env c)})))

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
  (eval [o env c] (succeed c o))

  clojure.lang.APersistentVector
  (eval [this env c]
    (reduce (mapv ast/->Immediate this) env c))

  janus.ast.Symbol
  (eval [this env c]
    (if-let [v (get env this)]
      (if (= v unboundmarker)
        (ast/->Immediate this)
        (succeed c v))
      ;; What carries its meaning on its back?
      (if-let [v (-> this meta :env (get this))]
        (reduce v env c)
        (println ("ERROR: unbound symbol: " this)))))

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
      (reduce head env (rt/withcc c {:return next}))))

  janus.ast.Mu
  (apply [head tail env c])

  janus.ast.PrimitiveMacro
  (apply [head tail env c]
    ((:f head) tail env c))

  janus.ast.PrimitiveFunction
  (apply [head tail env c]
    (succeed c (if (reduced? tail)
                 (apply (:f head) tail)
                 (ast/->Application head tail)))))
