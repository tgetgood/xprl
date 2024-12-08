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

(defn emit [ctx & args]
  (clojure.core/apply rt/emit (:continuations ctx) args))

(defmulti reduce* (fn [ctx] (type (:form ctx))))
(defmulti eval*   (fn [ctx] (type (:form ctx))))
(defmulti apply*  (fn [ctx] (type (:head ctx))))

(def ^:dynamic reduce reduce*)
(def ^:dynamic eval eval*)
(def ^:dynamic apply apply*)

(defmulti envwrapper (fn [form dyn] (type form)))

(defmacro add-method [multi [t args body]]
  `(defmethod multi ~t ~args ~body))

(defmacro impls
  {:style/indent [1]}
  [multi & body]
  `(do ~@(into [] (map (fn [[t args body]]
                         `(defmethod ~multi ~t ~args ~body)))
               (partition 3 body))))

(defn return [ctx f]
  (assoc-in ctx [:continuations rt/return] f))

(defn validate-μ [args]
  (condp = (count args)
    3 args
    2 [::anon (first args) (second args)]))

(defn createμ [{:keys [tail dyn continuations] :as ctx}]
  (let [[name params body] (validate-μ tail)]
    (reduce
     (-> ctx
         (assoc :form params :previous ctx)
         (assoc-in [:continuations ]))))
  )

(impls reduce*
  Object [{:keys [form] :as ctx}]
  (emit ctx rt/return form)

  janus.ast.Immediate [{:keys [form]}]
  {:call (eval form)}

  janus.ast.Application [{:keys [head tail]}]
  {:call (reduce head)
   :next (fn [head'] {:call (apply head' tail)})}
  )

(impls envwrapper
  :default [form dyn]

  )

(defn reduce-with [tx ctx]
  (binding [reduce (fn [ctx] (reduce* (tx ctx)))]
    (reduce ctx)))

#_(def evals
  {Object              (fn [form] {:emit [rt/return form]})
   janus.ast.Immediate (fn [{:keys [form]}]
                         {:eval form
                          :next (fn [form'] {:eval form'})})})
