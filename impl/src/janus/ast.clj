(ns janus.ast
  (:refer-clojure
   :exclude
   [symbol symbol? keyword keyword? destructure type list list?])
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str])

  (:import
   (java.io Writer)))

;;;;; Context

(defprotocol Contextual)

(extend-protocol Contextual
  clojure.lang.PersistentVector)

(defprotocol Symbolic
  (symbols [this]))

(extend-protocol Symbolic
  nil
  (symbols [_] #{})

  clojure.lang.PersistentVector
  (symbols [xs]
    (into #{} (mapcat symbols) xs))

  Object
  (symbols [_] #{}))

(defn contextual? [x]
  (satisfies? Contextual x))

;;;;; AST

(defn split-symbolic [s]
  (cond
    ;; REVIEW: The `.` syntax is too basic to allow it to be overridden, so `.`
    ;; probably shouldn't be a normal symbol. I don't see any use at the moment
    ;; for `..`, `...`, &c. as they're not good names so we might be better off
    ;; without them.
    (re-find #"^\.+$" s)  [s] ; `.`, `...`, &c. are valid symbols.
    ;; TODO: check we don't have empty ns portions: `...a...b...
    (str/includes? s ".") (str/split s #"\.")
    true                  [s]))

;; Keywords are values, which is to say they're context free
(defrecord Keyword [names]
  Object
  (toString [_]
    (transduce (interpose ".") str ":" names)))

(defn keyword? [k]
  (instance? Keyword k))

(def keyword
  (memoize (fn [s] (->Keyword (split-symbolic s)))))

(defrecord Symbol [names]
  Contextual
  Symbolic
  (symbols [this] #{this})
  Object
  (toString [_]
    (transduce (interpose ".") str "" names)))

(def symbol
    (memoize (fn [s] (->Symbol (split-symbolic s)))))

(defn symbol? [s]
  (instance? Symbol s))


(defn elements [l]
  l)

(defn list [xs]
  (into [] xs))

(defn list? [x]
  (vector?  x))

(defrecord Pair [head tail]
  Contextual
  Symbolic
  (symbols [_] (set/union (symbols head) (symbols tail)))
  Object
  (toString [_]
    (str "(" (str head) " "
         (if (list? tail)
           (transduce (comp (map str) (interpose " ")) str "" (elements tail))
           (str ". " (str tail)))
         ")")))

(defn pair [head tail]
  (->Pair head tail))

(defn pair? [x]
  (instance? Pair x))

(defrecord Immediate [form]
  Contextual
  Symbolic
  (symbols [_] (symbols form))
  Object
  (toString [_]
    (str "~" form)))

(defn immediate [form]
  (->Immediate form))

(defn immediate? [x]
  (instance? Immediate x))


(defrecord Application [head tail]
  Contextual
  Symbolic
  (symbols [_] (set/union (symbols head) (symbols tail)))
  Object
  (toString [_]
    (str "#" (str (pair head tail)))))

(defn application [head tail]
  (->Application head tail))

(defn application? [x]
  (instance? Application x))


(defrecord Mu [name params body]
  Contextual
  Symbolic
  ;; This is unintuitive, but we only look at the body because it ~might not~
  ;; refer to the name and formal param of the μ.
  (symbols [_] (symbols body))
  Object
  (toString [_]
    (str "(#μ " params " " body ")")))

(defn μ [name params body]
  (->Mu name params body))

(defn μ? [x]
  (instance? Mu x))


(defn fname [f]
  (let [s (:name (meta f))]
    (cond
      (clojure.core/symbol? s) (name s)
      (string? s)              s
      true                     (str f))))

(defrecord Primitive [check fn]
  Object
  (toString [_]
    (str "#F[" (fname fn) "]")))

(defn primitive [p f]
  (->Primitive p f))



(defrecord Macro [f]
  Object
  (toString [_]
    (str "#M[" (fname f) "]")))

(defn macro [f]
  (->Macro f))


(defrecord Nu [name params body]
  Contextual
  Symbolic
  Object
  (toString [_]
    (str "(#ν " params " " body ")")))


(defrecord Emission [kvs]
  Contextual
  Symbolic
  (symbols [_] (symbols kvs))
  Object
  (toString [_]
    (str "#E" kvs)))

(defn emission [kvs]
  (->Emission kvs))

(defn emission? [x]
  (instance? Emission x))

;;;;; Pretty Printing
;;
;; This comprises so much messy logic that I'm going to dump it all here to keep
;; it out of the way.

;; Boilerplate reducer.
(defmacro ps [type]
  `(do (defmethod print-method ~type [o# ^Writer w#]
         (.write w# (str o#)))))
;;; Symbol

(ps Symbol)

(defmethod pp/simple-dispatch Symbol [o]
  (pp/write-out (clojure.core/symbol (str o))))

;;; Keyword

(ps Keyword)

(defmethod pp/simple-dispatch Keyword [o]
  (pp/write-out (clojure.core/keyword (subs (str o) 1))))

;;; Pair

(defmethod print-method Pair [o ^Writer w]
  (.write w "(")
  (print-method (:head o) w)
  (if (list? (:tail o))
    (doseq [x (elements (:tail o))]
      (.write w " ")
      (print-method x w))
    (do
      (.write w " . ")
      (print-method (:tail o) w)))
  (.write w ")"))

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
    (pp/write-out (clojure.core/nth tail 2))))

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
   (if (list? tail)
     (format-pair head (elements tail))
     (do
       (.write ^Writer *out* " . ")
       (pp/write-out tail)))))

;;; Immediate

(ps Immediate)

(defmethod pp/simple-dispatch Immediate [i]
  ;; REVIEW: Is this advisable?
  (.write ^Writer *out* "~")
  (pp/write-out (:form i)))

;;; Application

(ps Application)

(defmethod pp/simple-dispatch Application [{:keys [head tail]}]
  ;; REVIEW: Is this advisable?
  (.write ^Writer *out* "#")
  (pp/simple-dispatch (pair head tail)))

;;; μ

(ps Mu)

(defmethod pp/simple-dispatch Mu [{:keys [params body]}]
  (pp/pprint-logical-block
   :prefix "(" :suffix ")"
   (pp/write-out (symbol "#μ"))
   (format-pair (symbol "#μ") [params body])))

;;; Primitive

(defmethod print-method Primitive [{:keys [fn]} ^Writer w]
  (.write w "#F[")
  (if-let [n (:name (meta fn))]
    (.write w (str n))
    (print-method fn w))
  (.write w "]"))

(defmethod pp/simple-dispatch Primitive [{:keys [fn]}]
  (pp/pprint-logical-block
   :prefix "#F[" :suffix "]"
   (if-let [n (:name (meta fn))]
     (pp/write-out (clojure.core/symbol n))
     (pp/write-out fn))))

;;; Macro

(defmethod print-method Macro [{:keys [f]} ^Writer w]
  (.write w "#M[")
  (if-let [n (:name (meta f))]
    (.write w (str n))
    (print-method f w))
  (.write w "]"))

(defmethod pp/simple-dispatch Macro [{:keys [f]}]
  (pp/pprint-logical-block
   :prefix "#M[" :suffix "]"
   (if-let [name (:name (meta f))]
     (pp/write-out name)
     (pp/write-out f))))

;;; Emission

(defmethod print-method Emission [{:keys [kvs]} ^Writer w]
  (.write w "#E")
  (print-method kvs w))

(defmethod pp/simple-dispatch Emission [{:keys [kvs]}]
  (pp/write-out (symbol "#E"))
  (pp/simple-dispatch kvs))

;;;;; Destructuring
;;
;; Destructuring is no longer implemented around the language. But I'm keeping
;; this around as reference for the implementation of destructuing ~in~ xprl at
;; some point.

(defprotocol Destructurable
  (binding? [this] "Is this form an admissible lhs to bind?")
  (bindings [this] "Returns a seq of symbols to be bound.")
  ;; TODO: Use clojure's `destructure`. I just don't fully understand it and
  ;; don't need its full power as yet. That might change.
  (destructure [this args]))

(extend-protocol Destructurable
  Symbol
  (binding? [_] true)
  (bindings [x] [x])
  (destructure [x y] {x y})

  ;; TODO: dot operator [x y . tail]
  clojure.lang.PersistentVector
  (binding? [x] (every? binding? x))
  (bindings [x] (into [] (comp (map bindings) cat ) x))
  (destructure [x y]
    (if (or (not (sequential? y)) (not= (count x) (count y)))
      nil
      (reduce merge (map destructure x y))))

  Object
  (binding? [_] false)
  (bindings [_] [])
  (destructure [xs ys] nil))

;;;;; Inspection

(defn spacer [^Writer w level]
  (dorun (map #(.write w ^String %) (take level (repeat "| ")))))

(defprotocol Inspectable
  (insp [form w level]))

(extend-protocol Inspectable
  Object
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "V[")
    (.write w (str form))
    (.write w "]\n"))

  Pair
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "P\n")
    (insp (:head form) w (inc level))
    (insp (:tail form) w (inc level)))

  Immediate
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "I\n")
    (insp (:form form) w (inc level)))

  Symbol
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "S[")
    (.write w (str form))
    (.write w "]\n"))

  Application
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "A\n")
    (insp (:head form) w (inc level))
    (insp (:tail form) w (inc level)))

  clojure.lang.PersistentVector
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "L\n")
    (dorun (map #(insp % w (inc level)) form)))

  Macro
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "M[")
    (.write w (str (:name (meta (:f form)))))
    (.write w "]\n"))

  Primitive
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "F[")
    (.write w (fname (:fn form)))
    (.write w "]\n"))

  Mu
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "μ\n")
    (insp (:params form) w (inc level))
    (insp (:body form) w (inc level)))

  Emission
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "E\n")
    (loop [kvs (elements (:kvs form))]
      (when (seq kvs)
        (insp (first kvs) w (inc level))
        (insp (second kvs) w (inc level))
        (recur (drop 2 kvs))))))


(defn inspect [x]
  (insp x *out* 0))

;;;;; Sugar

(def type-table
  {clojure.lang.PersistentVector :L

   Immediate   :I
   Pair        :P
   Symbol      :S
   Application :A
   Primitive   :F
   Macro       :M
   Mu          :μ
   Emission    :E})

(defn type [x]
  ;; There's nothing to gain in wrapping value types.
  (get type-table (clojure.core/type x) :V))

(def xkeys
  {:return  (keyword "return")
   :error   (keyword "error")
   :unbound (keyword "unbound")
   :env     (keyword "env")})
