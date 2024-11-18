(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:import [janus.ast Immediate])
  (:require [janus.ast :as ast]
            [janus.runtime :as rt]
            [janus.util :refer [fatal-error!]]
            [taoensso.telemere :as t]))

(defn ni [] (throw (RuntimeException. "not implemented")))
(def return (ast/keyword "return"))

(def unboundmarker
  "Value used by μs to indicate an env var is to be bound. Needed to shadow
  lexical env with params."
  (gensym))

(defn succeed [c v]
  (rt/emit c (ast/keyword "return") v))

(defn createμ [args env c]

  (succeed c 42)
)

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
  (reduce [o env c]
    (t/event! :reduce/fallthrough {:data o :level :trace})
    (succeed c o))

  clojure.lang.APersistentVector
  (reduced? [x] (every? reduced? x))
  (reduce [this env c]
    (t/event! :reduce/vector {:data this :level :trace})
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
    (t/event! :reduce/Immediate {:data x :level :trace})
    (eval (:form x) env c))

  janus.ast.Application
  (reduced? [_] false)
  (reduce [x env c]
    (t/event! :reduce/Application {:data x :level :trace})
    (reduce (:head x) env (rt/withcc c {return #(apply % (:tail x) env c)})))

  janus.ast.PartialMu
  (reduced? [_] false)
  (reduce [x env c]
    (t/event! :reduce/PartialMu {:data x :level :trace})
    (createμ [(:params x) (:body x)] env c))

  janus.ast.Mu
  (reduced? [x] (reduced? (:body x)))
  (reduce [x env c]
    (t/event! :reduce/Mu {:data x :level :trace})
    (createμ [(:params x) (:body x)] env c))

  janus.ast.Pair
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x))))
  (reduce [x env c]
    (t/event! :reduce/Pair {:data x :level :trace})
    (succeed c x))

  janus.ast.TopLevel
  (reduced? [x] (reduced? (:form x)))
  (reduce [{:keys [form]} env c]
    (succeed c form)))

(extend-protocol Evaluable
  Object
  (eval [o env c]
    (println (type o))
    (t/event! :eval/fallthrough {:data o :level :trace})
    (succeed c o))

  clojure.lang.APersistentVector
  (eval [this env c]
    (t/event! :eval/Vector {:data this :level :trace})
    (reduce (mapv ast/->Immediate this) env c))

  janus.ast.Symbol
  (eval [this env c]
    (t/event! :eval/Symbol
              {:data  {:symbol this :dyn env :lex (-> this meta :env)}
               :level :trace})
    (if-let [v (get env this)]
      (if (= v unboundmarker)
        (ast/->Immediate this)
        (reduce v env c))
      ;; What carries its meaning on its back?
      (if-let [v (-> this meta :env (get this))]
        (reduce v env c)
        (fatal-error! this "unbound symbol"))))

  janus.ast.Pair
  (eval [{:keys [head tail] :as this} env c]
    (t/event! :eval/Pair {:data this :level :trace})
    (reduce (ast/->Application (ast/->Immediate head) tail) env c))

  janus.ast.Immediate
  (eval [{:keys [form] :as this} env c]
    (t/event! :eval/Immediate {:data this :level :trace})
    (letfn [(next [form]
              (if (instance? janus.ast.Immediate form)
                (succeed c (ast/->Immediate form))
                (eval form env c)))]
      (reduce form env (rt/withcc c return next))))

  janus.ast.Application
  (eval [form env c]
    (t/event! :eval/Application {:data form :level :trace})
    (letfn [(next [form]
              (if (instance? janus.ast.Application form)
                (succeed c (ast/->Immediate form))
                (eval form env c)))]
      (reduce form env (rt/withcc c return next)))))

(extend-protocol Applicable
  janus.ast.Immediate
  ;; Immediates cannot be applied. This is a delay tactic.
  (apply [head tail env c]
    (t/event! :apply/Immediate {:data [head tail] :level :trace})
    (succeed c (ast/->Application head tail)))

  janus.ast.Application
  (apply [head tail env c]
    (t/event! :apply/Application {:data [head tail] :level :trace})
    (letfn [(next [head]
              (if (instance? janus.ast.Application head)
                (succeed c (ast/->Application head tail))
                (apply head tail env c)))]
      (reduce head env (rt/withcc c {return next}))))

  janus.ast.Mu
  (apply [head tail env c]
    (t/event! :apply/Mu {:data [head tail] :level :trace})
    (ni))

  janus.ast.PartialMu
  (apply [head tail env c]
    (t/event! :apply/PartialMu {:data [head tail] :level :trace})
    (ni))

  janus.ast.PrimitiveMacro
  (apply [head tail env c]
    (t/event! :apply/Macro {:data [head tail (meta tail)] :level :trace})
    ((:f head) tail env c))

  janus.ast.PrimitiveFunction
  (apply [head tail env c]
    (t/event! :apply/Fn {:data [head tail] :level :trace})
    (letfn [(next [tail]
              (succeed c (if (reduced? tail)
                           (clojure.core/apply (:f head) tail)
                           (ast/->Application head tail))))]
      (reduce tail env (rt/withcc c return next)))))
