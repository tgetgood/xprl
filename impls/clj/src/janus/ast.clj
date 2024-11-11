(ns janus.ast
  (:refer-clojure :exclude [reduced? symbol keyword])
  (:require [clojure.string :as str]))

;;;;; Base types

(deftype True [])
(deftype False [])

(defonce t (True.))
(defonce f (False.))

(defrecord Keyword [names])
(defrecord Symbol [names])

(defonce dot (->Symbol ["."]))

(defn split-symbolic [s]
  (if (str/includes? s ".")
    (str/split s #"\.")
    [s]))

(defn symbol [s]
  (cond
    ;; TODO: Intern symbols
    (= s ".")          dot
    (re-find #"\.+" s) (->Symbol [s])
    :else              (->Symbol (split-symbolic s))))

(defn keyword [s]
  (->Keyword (split-symbolic s)))

;;;;; Reader AST

(defprotocol INode
  (reduced? [this]
    "Has this form been fully reduced?"))

(defrecord Pair [head tail]
  INode
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x)))))

(defrecord Immediate [form]
  INode
  (reduced? [_] false))

(defrecord Application [head tail]
  INode
   (reduced? [_] false))

(defrecord PartialMu [params body]
  INode
  (reduced? [_] false))

(defrecord Mu [env source params body]
  INode
  (reduced? [x] (reduced? (:body x))))

(defrecord PrimitiveFunction [f])

(defrecord PrimitiveMacro [f])

(extend-protocol INode
  Object
  (reduced? [_] true)

  clojure.lang.ASeq
  (reduced? [x] (transduce (map reduced?) #(and %1 %2) true x))

  clojure.lang.AMapEntry
  (reduced? [x] (and (reduced? (key x)) (reduced? (val x))))

  clojure.lang.APersistentMap
  (reduced? [x] (reduced? (seq x)))

  clojure.lang.APersistentSet
  (reduced? [x] (reduced? (seq x)))

  clojure.lang.APersistentVector
  (reduced? [x] (reduced? (seq x))))
