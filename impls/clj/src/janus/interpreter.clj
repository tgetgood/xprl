(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:require [clojure.pprint :as pp]
            [clojure.walk :as walk]
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

;; FIXME: just find all calls to this
(defn ni [] (throw (RuntimeException. "not implemented")))

(defn tag-meta [o op prev]
  (if (instance? clojure.lang.IObj o)
    (with-meta o (assoc (meta o) :history/operation op :history/value prev))
    o))

(defn dyn-form
  [x]
  {:form x :dyn (-> x meta :dyn)})

(defn event!
  [id m]
  (t/event! id {:level :trace :data m}))

(defn succeed [c v]
  (rt/emit c rt/return v))

(defrecord Unbound [mark])

(defmethod pp/simple-dispatch Unbound [{:keys [mark]}]
  (pp/write-out "unbound[")
  (pp/simple-dispatch mark)
  (pp/write-out "]"))

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
                                                   :history/operation :createμ
                                                   :history/value args
                                                   :source body
                                                   :marker m))))]
                   (event! :createμ/bound {:params params :body body} )
                   (reduce body' (rt/withcc c rt/return next)))
                 (do
                   (event! :createμ/unbound {:params params :body body})
                   (succeed c (with-meta (ast/->PartialMu params body)
                                (assoc (meta args)
                                       :history/operation :createμ
                                       :history/value args))))))]
    (reduce params (rt/withcc c rt/return next))))

(extend-protocol Reductive
  Object
  (reduced? [_] true)
  (reduce [o c]
    (event! :reduce/fallthrough (dyn-form o))
    (succeed c (tag-meta o :reduce/fallthrough ::unchanged)))

  clojure.lang.APersistentVector
  (reduced? [x] (every? reduced? x))
  (reduce [this c]
    (event! :reduce/vector {:data this})
    (let [next (fn [v] (succeed c (tag-meta v :reduce/vector this)))
          collector (rt/collector (rt/withcc c rt/return next) (count this))
          runner (fn [[i x]]
                   (reduce
                    x
                    (rt/withcc c rt/return (fn [v] (rt/receive collector i v)))))
          tasks (interleave (repeat runner) (map-indexed vector this))]
      (event! :reduce.vector/forks tasks)
      (clojure.core/apply rt/emit c tasks)))

  clojure.lang.AMapEntry
  (reduced? [x] (and (reduced? (key x)) (reduced? (val x))))
  (reduce [this c] (ni))

  clojure.lang.APersistentMap
  (reduced? [x] (every? reduced? x))
  (reduce [this c] (ni))

  clojure.lang.APersistentSet
  (reduced? [x] (every? reduced? x))
  (reduce [this c] (ni))


  janus.ast.Immediate
  (reduced? [_] false)
  (reduce [x c]
    (event! :reduce/Immediate (dyn-form x))
    (eval (tag-meta (:form x) :reduce/Immediate x) c))

  janus.ast.Application
  (reduced? [_] false)
  (reduce [x c]
    (event! :reduce/Application (dyn-form x))
    (reduce (tag-meta (:head x) :reduce/Application x)
            (rt/withcc c rt/return #(apply % (:tail x) c))))

  janus.ast.PartialMu
  (reduced? [_] false)
  (reduce [x c]
    (event! :reduce/PartialMu (dyn-form x))
    (createμ [(:params x) (:body x)] c))

  janus.ast.Mu
  (reduced? [x] (reduced? (:body x)))
  (reduce [x c]
    (event! :reduce/Mu (dyn-form x))
    (createμ [(:params x) (:body x)] c))

  janus.ast.Pair
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x))))
  (reduce [x c]
    (event! :reduce/Pair (dyn-form x))
    (succeed c (tag-meta x :reduce/Pair ::unchanged))))

(extend-protocol Evaluable
  Object
  (eval [o c]
    (event! :eval/fallthrough (assoc (dyn-form o) :type (type o)))
    (succeed c (tag-meta o :eval/fallthrough ::unchanged)))

  clojure.lang.APersistentVector
  (eval [this c]
    (event! :eval/Vector (dyn-form this))
    (reduce (tag-meta (mapv ast/immediate this) :eval/Vector this) c))

  janus.ast.Symbol
  (eval [this c]
    (event! :eval/Symbol {:symbol this :lex (-> this meta :lex keys)
                          :dyn (:dyn (meta this))} )
    (if-let [v (get (:dyn (meta this)) this)]
      (if (instance? Unbound v)
        (succeed c (ast/immediate this))
        (reduce (tag-meta v :eval.Symbol/dynamic this) c))
      ;; What has two backs and carries its meaning on each?
      (if-let [v (-> this meta :lex (get this))]
        (reduce (tag-meta v :eval.Symbol/lexical this) c)
        (fatal-error! c this "unbound symbol"))))

  janus.ast.Pair
  (eval [{:keys [head tail] :as this} c]
    (event! :eval/Pair {:data this})
    (reduce (tag-meta (ast/application (ast/immediate head) tail) :eval/Pair this) c))

  janus.ast.Immediate
  (eval [{:keys [form] :as this} c]
    (event! :eval/Immediate (dyn-form this))
    (letfn [(next [form]
              (if (instance? janus.ast.Immediate form)
                ;; Undo, don't trace execution.
                (succeed c (ast/immediate this))
                (eval (tag-meta form :eval/Immediate this) c)))]
      (eval form (rt/withcc c rt/return next))))

  janus.ast.Application
  (eval [form c]
    (event! :eval/Application (dyn-form form))
    (letfn [(next [form]
              (if (instance? janus.ast.Application form)
                (succeed c (ast/immediate form))
                (eval form c)))]
      (reduce form (rt/withcc c rt/return next)))))

(extend-protocol Applicable
  janus.ast.Immediate
  ;; Immediates cannot be applied yet. This is a delay tactic.
  (apply [head tail c]
    (event! :apply/Immediate {:data [head tail]})
    (succeed c (ast/application head tail)))

  janus.ast.Application
  (apply [head tail c]
    (event! :apply/Application {:data [head tail]})
    (letfn [(next [head']
              (if (instance? janus.ast.Application head)
                (succeed c (ast/application head' tail))
                (apply (tag-meta head' :apply/Application head) tail c)))]
      (reduce head (rt/withcc c rt/return next))))

  janus.ast.Mu
  (apply [head tail c]
    (event! :apply/Mu {:head (dyn-form head) :tail (dyn-form tail)})
    (letfn [(next [tail']
              (let [bind (ast/destructure (:params head) tail')]
                (event! :apply.Mu/destructuring bind)
                (if (nil? bind)
                  (succeed c (ast/application head tail'))
                  ;; FIXME: Make sure the bindings have the correct Unbound so
                  ;; that we don't set lexically shadowed values.
                  (let [body (pushbind bind (:body head))]
                    (reduce body c)))))]
      (reduce tail (rt/withcc c rt/return next))))

  janus.ast.PartialMu
  (apply [head tail c]
    (event! :apply/PartialMu {:data [head tail]})
    (succeed c (ast/application head tail)))

  janus.ast.PrimitiveMacro
  (apply [head tail c]
    (event! :apply/Macro [head tail (dissoc (meta tail) :lex)])
    ((:f head) tail c))

  janus.ast.PrimitiveFunction
  (apply [head tail c]
    (event! :apply/Fn {:data [head tail]})
    (letfn [(next [tail]
              (succeed c (if (reduced? tail)
                           (clojure.core/apply (:f head) tail)
                           (ast/application head tail))))]
      (reduce tail (rt/withcc c rt/return next)))))

;; I think that I want what I'm calling the dynamic environment (thinks passed
;; as arguments) to be static, but I still want the continuations to follow
;; dynamic scoping. The whole point of named channels is that a μ doesn't know
;; what's connected to them, so the semantics aren't violated if that gets
;; changed between the time the μ is defined and the time messages actually get
;; sent.
