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

(defmulti reduce
  "Syntactically reduce an expression in a given dynamic environment."
  (fn [form dyn continuations opts] (type form)))

(defmethod reduce Object [form dyn cs opts]
  {:return form})

;; (defmethod reduce clojure.lang.PersistentVector)
