(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:import [janus.ast Immediate])
  (:require [janus.ast :as ast]
            [janus.runtime :as rt]
            [janus.util :refer [fatal-error!]]
            [taoensso.telemere :as t]))

(defprotocol Reductive
  (reduced? [this])
  (reduce [this env c] {:style/indent 2}))

(defprotocol Evaluable
  (eval [this env c]))

(defprotocol Applicable
   (apply [this args env c]))

(defn ni [] (throw (RuntimeException. "not implemented")))

(defn event!
  [id m]
  (t/event! id {:level :trace :data m}))

(def marker
  "Value used by μs to indicate an env var is to be bound. Needed to shadow
  lexical env with params."
  (gensym))

(defn mark [m s]
  (assoc m s marker))

(defn succeed [c v]
  (rt/emit c rt/return v))

(defn createμ [args env c]
  (let [[params body] args
        next (fn [params]
               (if (ast/binding? params)
                 (let [env' (clojure.core/reduce mark env (ast/bindings params))
                       next (fn [cbody]
                              (succeed c (ast/->Mu env body params cbody)))]
                   (event! :createμ/bound {:params params :body body :dyn env'} )
                   (reduce body env' (rt/withcc c rt/return next)))
                 (do
                   (event! :createμ/unbound {:params params :body body})
                   (succeed c (ast/->PartialMu params body)))))]
    (reduce params env (rt/withcc c rt/return next))))

(extend-protocol Reductive
  Object
  (reduced? [_] true)
  (reduce [o env c]
    (event! :reduce/fallthrough {:data o :dyn env})
    (succeed c o))

  clojure.lang.APersistentVector
  (reduced? [x] (every? reduced? x))
  (reduce [this env c]
    (event! :reduce/vector {:data this :dyn env})
    (let [collector (rt/collector c (count this))
          runner (fn [[i x]]
                   (reduce
                    x
                    env
                    (rt/withcc c rt/return (fn [v] (rt/receive collector i v)))))
          tasks (interleave (repeat runner) (map-indexed vector this))]
      (event! :reduce.vector/forks tasks)
      (clojure.core/apply rt/emit c tasks)))

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
    (event! :reduce/Immediate {:data x :dyn env})
    (eval (:form x) env c))

  janus.ast.Application
  (reduced? [_] false)
  (reduce [x env c]
    (event! :reduce/Application {:data x :dyn env})
    (reduce (:head x) env (rt/withcc c rt/return #(apply % (:tail x) env c))))

  janus.ast.PartialMu
  (reduced? [_] false)
  (reduce [x env c]
    (event! :reduce/PartialMu {:data x :dyn env})
    (createμ [(:params x) (:body x)] env c))

  janus.ast.Mu
  (reduced? [x] (reduced? (:body x)))
  (reduce [x env c]
    (event! :reduce/Mu {:data x :dyn env})
    (createμ [(:params x) (:body x)] env c))

  janus.ast.Pair
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x))))
  (reduce [x env c]
    (event! :reduce/Pair {:data x :dyn env})
    (succeed c x))

  janus.ast.TopLevel
  (reduced? [x] (reduced? (:form x)))
  (reduce [{:keys [form]} env c]
    (succeed c form)))

(extend-protocol Evaluable
  Object
  (eval [o env c]
    (event! :eval/fallthrough {:value o :type (type o) :dyn env})
    (succeed c o))

  clojure.lang.APersistentVector
  (eval [this env c]
    (event! :eval/Vector {:data this :dyn env})
    (reduce (mapv ast/->Immediate this) env c))

  janus.ast.Symbol
  (eval [this env c]
    (event! :eval/Symbol {:symbol this :dyn env :lex (-> this meta :env keys)})
    (if-let [v (get env this)]
      (if (= v marker)
        (succeed c (ast/->Immediate this))
        (reduce v env c))
      ;; What carries its meaning on its back?
      (if-let [v (-> this meta :env (get this))]
        (reduce v env c)
        (fatal-error! c this "unbound symbol"))))

  janus.ast.Pair
  (eval [{:keys [head tail] :as this} env c]
    (event! :eval/Pair {:data this :dyn env})
    (reduce (ast/->Application (ast/->Immediate head) tail) env c))

  janus.ast.Immediate
  (eval [{:keys [form] :as this} env c]
    (event! :eval/Immediate {:data this :dyn env})
    (letfn [(next [form]
              (if (instance? janus.ast.Immediate form)
                (succeed c (ast/->Immediate this))
                (eval form env c)))]
      (eval form env (rt/withcc c rt/return next))))

  janus.ast.Application
  (eval [form env c]
    (event! :eval/Application {:data form :dyn env})
    (letfn [(next [form]
              (if (instance? janus.ast.Application form)
                (succeed c (ast/->Immediate form))
                (eval form env c)))]
      (reduce form env (rt/withcc c rt/return next)))))

(extend-protocol Applicable
  janus.ast.Immediate
  ;; Immediates cannot be applied. This is a delay tactic.
  (apply [head tail env c]
    (event! :apply/Immediate {:data [head tail] :dyn env})
    (succeed c (ast/->Application head tail)))

  janus.ast.Application
  (apply [head tail env c]
    (event! :apply/Application {:data [head tail] :dyn env})
    (letfn [(next [head]
              (if (instance? janus.ast.Application head)
                (succeed c (ast/->Application head tail))
                (apply head tail env c)))]
      (reduce head env (rt/withcc c rt/return next))))

  janus.ast.Mu
  (apply [head tail env c]
    (event! :apply/Mu {:data [head tail] :dyn env})
    (letfn [(next [tail]
              (let [bind (ast/destructure (:params head) tail)]
                (event! :apply.Mu/destructuring bind)
                (if (nil? bind)
                  (succeed c (ast/->Application head tail))
                  (reduce (:body head) (merge env bind) c))))]
      (reduce tail env (rt/withcc c rt/return next))))

  janus.ast.PartialMu
  (apply [head tail env c]
    (event! :apply/PartialMu {:data [head tail] :dyn env})
    (succeed c (ast/->Application head tail)))

  janus.ast.PrimitiveMacro
  (apply [head tail env c]
    (event! :apply/Macro {:data [head tail (dissoc (meta tail) :env)] :dyn env})
    ((:f head) tail env c))

  janus.ast.PrimitiveFunction
  (apply [head tail env c]
    (event! :apply/Fn {:data [head tail] :dyn env})
    (letfn [(next [tail]
              (succeed c (if (reduced? tail)
                           (clojure.core/apply (:f head) tail)
                           (ast/->Application head tail))))]
      (reduce tail env (rt/withcc c rt/return next)))))
