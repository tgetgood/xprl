(ns janus.i2
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:require
   [janus.ast :as ast]
   [janus.runtime :as rt]
   [janus.util :refer [fatal-error!]]
   [taoensso.telemere :as t]))

(defn reduced? [x]
  (condp instance? x
    clojure.lang.PersistentVector (every? reduced x)
    clojure.lang.AMapEntry        (and (reduced? (key x)) (reduced? (val x)))
    clojure.lang.APersistentMap   (every? reduced? x)
    clojure.lang.APersistentSet   (every? reduced? x)
    janus.ast.Immediate           false
    janus.ast.Application         false
    janus.ast.Pair                (and (reduced? (:head x)) (reduced? (:tail x)))

    true))

(declare emit)

(defmulti reduce (fn [form] (type form)))
(defmulti eval (fn [form] (type form)))
(defmulti apply (fn [head tail] (type head)))
(defmulti envwrapper (fn [form dyn] (type form)))

(defmacro add-method [multi [t args body]]
  `(defmethod multi ~t ~args ~body))

(defmacro impls
  {:style/indent [1]}
  [multi & body]
  `(do ~@(into [] (map (fn [[t args body]]
                         `(defmethod ~multi ~t ~args ~body)))
               (partition 3 body))))

(impls reduce
  Object [form]
  {:call (emit rt/return form)}

  janus.ast.Immediate [{:keys [form]}]
  {:call (eval form)}

  janus.ast.Application [{:keys [head tail]}]
  {:call (reduce head)
   :next (fn [head'] {:call (apply head' tail)})}
  )

(impls envwrapper
  :default [form dyn]

  )
#_(def evals
  {Object              (fn [form] {:emit [rt/return form]})
   janus.ast.Immediate (fn [{:keys [form]}]
                         {:eval form
                          :next (fn [form'] {:eval form'})})})
