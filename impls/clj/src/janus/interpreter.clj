(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:import [janus.ast Immediate])
  (:require [clojure.walk :as walk]
            [janus.ast :as ast]
            [janus.runtime :as rt]
            [janus.util :refer [fatal-error!]]
            [taoensso.telemere :as t]))

(defprotocol Reductive
  (reduced? [this])
  (reduce [this c]))

(defprotocol Evaluable
  (eval [this c]))

(defprotocol Applicable
   (apply [this args c]))

(defn ni [] (throw (RuntimeException. "not implemented")))

(defn event!
  [id m]
  (t/event! id {:level :trace :data m}))

(defn succeed [c v]
  (rt/emit c rt/return v))

(defrecord Unbound [mark])

(defn marker []
  (->Unbound (gensym)))

(defn pushbind [mask tree]
  (walk/postwalk (fn [form]
                   (if (instance? clojure.lang.IObj form)
                     (with-meta form
                       (update (meta form) :dyn merge mask))
                     form))
                 tree))

(defn unbind [syms marker tree]
  (pushbind (into {} (map (fn [s] [s marker])) syms) tree))


(defn createμ [args c]
  (let [[params body] args

        next (fn [params]
               (if (ast/binding? params)
                 (let [m     (marker)
                       body' (unbind (ast/bindings params) m body)
                       next  (fn [cbody]
                               (succeed c (with-meta (ast/->Mu params cbody)
                                            (assoc (meta args)
                                                   :source body))))]
                   (event! :createμ/bound {:params params :body body} )
                   (reduce body' (rt/withcc c rt/return next)))
                 (do
                   (event! :createμ/unbound {:params params :body body})
                   (succeed c (ast/->PartialMu params body)))))]
    (reduce params (rt/withcc c rt/return next))))

(extend-protocol Reductive
  Object
  (reduced? [_] true)
  (reduce [o c]
    (event! :reduce/fallthrough {:data o})
    (succeed c o))

  clojure.lang.APersistentVector
  (reduced? [x] (every? reduced? x))
  (reduce [this c]
    (event! :reduce/vector {:data this})
    (let [collector (rt/collector c (count this))
          runner (fn [[i x]]
                   (reduce
                    x
                    (rt/withcc c rt/return (fn [v] (rt/receive collector i v)))))
          tasks (interleave (repeat runner) (map-indexed vector this))]
      (event! :reduce.vector/forks tasks)
      (clojure.core/apply rt/emit c tasks)))

  clojure.lang.AMapEntry
  (reduced? [x] (and (reduced? (key x)) (reduced? (val x))))
  (reduce [this c]
    (ni))

  clojure.lang.APersistentMap
  (reduced? [x] (every? reduced? x))
  (reduce [this c]
    (ni))

  clojure.lang.APersistentSet
  (reduced? [x] (every? reduced? x))
  (reduce [this c]
    (ni))

  janus.ast.Immediate
  (reduced? [_] false)
  (reduce [x c]
    (event! :reduce/Immediate {:data x})
    (eval (:form x) c))

  janus.ast.Application
  (reduced? [_] false)
  (reduce [x c]
    (event! :reduce/Application {:data x})
    (reduce (:head x) (rt/withcc c rt/return #(apply % (:tail x) c))))

  janus.ast.PartialMu
  (reduced? [_] false)
  (reduce [x c]
    (event! :reduce/PartialMu {:data x})
    (createμ [(:params x) (:body x)] c))

  janus.ast.Mu
  (reduced? [x] (reduced? (:body x)))
  (reduce [x c]
    (event! :reduce/Mu {:data x})
    (createμ [(:params x) (:body x)] c))

  janus.ast.Pair
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x))))
  (reduce [x c]
    (event! :reduce/Pair {:data x})
    (succeed c x))

  janus.ast.TopLevel
  (reduced? [x] (reduced? (:form x)))
  (reduce [{:keys [form]} c]
    (succeed c form)))

(extend-protocol Evaluable
  Object
  (eval [o c]
    (event! :eval/fallthrough {:value o :type (type o)})
    (succeed c o))

  clojure.lang.APersistentVector
  (eval [this c]
    (event! :eval/Vector {:data this})
    (reduce (mapv ast/->Immediate this) c))

  janus.ast.Symbol
  (eval [this c]
    (event! :eval/Symbol {:symbol this :lex (-> this meta :lex keys)})
    (if-let [v (get (:dyn (meta this)) this)]
      (if (instance? Unbound v)
        (succeed c (ast/->Immediate this))
        (reduce v c))
      ;; What carries its meaning on its back?
      (if-let [v (-> this meta :lex (get this))]
        (reduce v c)
        (fatal-error! c this "unbound symbol"))))

  janus.ast.Pair
  (eval [{:keys [head tail] :as this} c]
    (event! :eval/Pair {:data this})
    (reduce (ast/->Application (ast/->Immediate head) tail) c))

  janus.ast.Immediate
  (eval [{:keys [form] :as this} c]
    (event! :eval/Immediate {:data this})
    (letfn [(next [form]
              (if (instance? janus.ast.Immediate form)
                (succeed c (ast/->Immediate this))
                (eval form c)))]
      (eval form (rt/withcc c rt/return next))))

  janus.ast.Application
  (eval [form c]
    (event! :eval/Application {:data form})
    (letfn [(next [form]
              (if (instance? janus.ast.Application form)
                (succeed c (ast/->Immediate form))
                (eval form c)))]
      (reduce form (rt/withcc c rt/return next)))))

(extend-protocol Applicable
  janus.ast.Immediate
  ;; Immediates cannot be applied. This is a delay tactic.
  (apply [head tail c]
    (event! :apply/Immediate {:data [head tail]})
    (succeed c (ast/->Application head tail)))

  janus.ast.Application
  (apply [head tail c]
    (event! :apply/Application {:data [head tail]})
    (letfn [(next [head]
              (if (instance? janus.ast.Application head)
                (succeed c (ast/->Application head tail))
                (apply head tail c)))]
      (reduce head (rt/withcc c rt/return next))))

  janus.ast.Mu
  (apply [head tail c]
    (event! :apply/Mu {:data [head tail]})
    (letfn [(next [tail]
              (let [bind (ast/destructure (:params head) tail)]
                (event! :apply.Mu/destructuring bind)
                (if (nil? bind)
                  (succeed c (ast/->Application head tail))
                  ;; FIXME: Make sure the bindings have the correct Unbound so
                  ;; that we don't set lexically shadowed values.
                  (let [body (pushbind bind (:body head))]
                    (reduce body c)))))]
      (reduce tail (rt/withcc c rt/return next))))

  janus.ast.PartialMu
  (apply [head tail c]
    (event! :apply/PartialMu {:data [head tail]})
    (succeed c (ast/->Application head tail)))

  janus.ast.PrimitiveMacro
  (apply [head tail c]
    (event! :apply/Macro {:data [head tail (dissoc (meta tail) :lex)]})
    ((:f head) tail c))

  janus.ast.PrimitiveFunction
  (apply [head tail c]
    (event! :apply/Fn {:data [head tail]})
    (letfn [(next [tail]
              (succeed c (if (reduced? tail)
                           (clojure.core/apply (:f head) tail)
                           (ast/->Application head tail))))]
      (reduce tail (rt/withcc c rt/return next)))))

;; I think that I want what I'm calling the dynamic environment (thinks passed
;; as arguments) to be static, but I still want the continuations to follow
;; dynamic scoping. The whole point of named channels is that a μ doesn't know
;; what's connected to them, so the semantics aren't violated if that gets
;; changed between the time the μ is defined and the time messages actually get
;; sent.
