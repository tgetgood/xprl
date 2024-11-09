(ns janus.ast)

(defprotocol INode
  (reduced? [this]
    "Has this form been fully reduced?"))

(defrecord Pair [head tail]
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x)))))

(defrecord Immediate [form]
  (reduced? [_] false))

(defrecord Application [head tail]
   (reduced? [_] false))

(defrecord PartialMu [params body]
  (reduced? [_] false))

(defrecord Mu [env source params body]
  (reduced? [x] (reduced? (:body x))))

(defrecord PrimitiveFunction [f])

(defrecord PrimitiveMacro [f])

(extend-protocol INode
  Object
  (reduced? [_] true)

  clojure.lang.ASeq
  (reduced? [x] (transduce (map reduced?) and true x))

  clojure.lang.AMapEntry
  (reduced? [x] (and (reduced? (key x)) (reduced? (val x))))

  clojure.lang.APersistentMap
  (reduced? [x] (reduced? (seq x)))

  clojure.lang.APersistentSet
  (reduced? [x] (reduced? (seq x)))

  clojure.lang.APersistentVector
  (reduced? [x] (reduced? (seq x))))
