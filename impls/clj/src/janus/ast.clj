(ns janus.ast)

;;;;; Base types

(deftype True [])
(deftype False [])

(defonce T (True.))
(defonce F (False.))

(defrecord Keyword [names])
(defrecord Symbol [names])

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
