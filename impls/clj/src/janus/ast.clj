(ns janus.ast
  (:refer-clojure :exclude [reduced? symbol keyword keyword?])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp])
  (:import [java.io Writer]))

;; Boilerplate reducer.
(defmacro ps [type]
  `(do (defmethod print-method ~type [o# ^Writer w#]
         (.write w# (str o#)))))

;; TODO: pp/simple-dispatch overrides how code is displayed in the repl.
;; TODO: and what does pp/code-dispatch do? Not 100% clear

;;;;; Base types

(deftype True [])
(deftype False [])

(defonce t (True.))
(defonce f (False.))

(defrecord Keyword [names]
  Object
  (toString [_]
    (transduce (interpose ".") str ":" names)))

(defn keyword? [k]
  (instance? Keyword k))

(defmethod pp/simple-dispatch Keyword [o]
  (pp/write-out (clojure.core/keyword (subs (str o) 1))))

(ps Keyword)

(defrecord Symbol [names]
  Object
  (toString [_]
    (transduce (interpose ".") str "" names)))

(defmethod pp/simple-dispatch Symbol [o]
  (pp/write-out (clojure.core/symbol (str o))))

(ps Symbol)

(defonce dot (->Symbol ["."]))

(defn split-symbolic [s]
  (if (str/includes? s ".")
    (str/split s #"\.")
    [s]))

(defn symbol [s]
  (cond
    ;; TODO: Intern symbols
    (= s ".")          dot
    (re-find #"^\.+$" s) (->Symbol [s])
    :else              (->Symbol (split-symbolic s))))

(defn keyword [s]
  (->Keyword (split-symbolic s)))

;;;;; Reader AST

(defprotocol INode
  (reduced? [this]
    "Has this form been fully reduced?"))

(defrecord Pair [head tail]
  Object
  (toString [_]
    (str "(" (str head) " "
         (if (vector? tail)
           (transduce (comp (map str) (interpose " ")) str "" tail)
           (str ". " (str tail)))
         ")"))
  INode
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x)))))

(ps Pair)

(defmulti format-pair (fn [head tail] head) :default :default)

(defmethod format-pair :default
  [_ tail]
  (when (seq tail)
    (pp/print-length-loop [tail (seq tail)]
                          (.write ^Writer *out* " ")
                          (pp/write-out (first tail))
                          (when (next tail)
                            (recur (next tail))))))

;; TODO: Add a param for the number of args to keep on line 1
;; TODO: read cider's format meta code and don't reinvent the wheel.
(defn pprint-block [tail]
  (.write ^Writer *out* " ")
  (pp/write-out (first tail))
  (pp/pprint-indent :block 1)
  (.write ^Writer *out* " ")
  (pp/pprint-newline :linear)
  (pp/write-out (second tail))
  (when (< 2 (count tail))
    (assert (< (count tail) 4))
    (.write ^Writer *out* " ")
    (pp/pprint-newline :linear)
    (pp/write-out (nth tail 2))))

(defmethod format-pair (symbol "def")
  [_ tail]
  (pprint-block tail))

(defmethod format-pair (symbol "fn")
  [_ tail]
  (pprint-block tail))

(defmethod format-pair (symbol "μ")
  [_ tail]
  (pprint-block tail))

(defmethod pp/simple-dispatch Pair [{:keys [head tail]}]
  ;; (pp/pprint-meta p)
  (pp/pprint-logical-block
   :prefix "(" :suffix ")"
   ;; TODO: Dispatch on head of pair to format
   (pp/write-out head)
   (if (vector? tail)
     (format-pair head tail)
     (do
       (.write ^Writer *out* " . ")
       (pp/write-out tail)))))

(defrecord Immediate [form]
  Object
  (toString [_]
    (str "~" form))
  INode
  (reduced? [_] false))

(ps Immediate)

(defmethod pp/simple-dispatch Immediate [i]
  ;; REVIEW: Is this advisable?
  (.write ^Writer *out* "~")
  (pp/write-out (:form i)))

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


(defmethod print-method Pair [o ^Writer w]
  (.write w "(")
  (print-method (:head o) w)
  (if (vector? (:tail o))
    (doseq [x (:tail o)]
      (.write w " ")
      (print-method x w))
    (do
      (.write w " . ")
      (print-method (:tail o) w)))
  (.write w ")"))
