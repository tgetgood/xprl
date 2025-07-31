(ns janus.ast
  (:refer-clojure
   :exclude
   [symbol
    symbol?
    keyword
    keyword?
    type
    list
    list?
    seq
    seq?
    map?
    set?
    resolve])
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

  clojure.lang.PersistentArrayMap
  (symbols [m]
    (into #{} (mapcat symbols) m))

  clojure.lang.PersistentHashMap
  (symbols [m]
    (into #{} (mapcat symbols) m))

  clojure.lang.PersistentHashSet
  (symbols [s]
    (into #{} (mapcat symbols) s))

  clojure.lang.MapEntry
  (symbols [[k v]]
    (set/union (symbols k) (symbols v)))

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

(defn unresolved? [s]
  (instance? Symbol s))


(defrecord Resolved [sym form]
  Contextual
  Symbolic
  (symbols [_] #{sym})
  Object
  (toString [_]
    (str sym "=" #_form)))

(defn resolve [sym val]
  (->Resolved sym val))

(defn resolved? [x]
  (instance? Resolved x))

(defn unresolve [x]
  (if (resolved? x)
    (:sym x)
    x))

(defn symbol? [s]
  (or
   (instance? Symbol s)
   (instance? Resolved s)))


(defn elements [l]
  l)

(defn list [xs]
  (into [] xs))

(defn list? [x]
  (vector?  x))

(defn map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn set? [x]
  (instance? clojure.lang.PersistentHashSet x))


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

(defn μ
  ([params body] (μ nil params body))
  ([name params body] (->Mu name params body)))

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



(defrecord Extern [name fn]
  Object
  (toString [_]
    (str "#F[" name "]")))

(defn extern [name fn]
  (->Extern name fn))


(defrecord Nu [params body]
  Contextual
  Symbolic
  (symbols [_] (symbols body))
  Object
  (toString [_]
    (str "(#ν " params " "  body ")")))

(defn ν [params body]
  (->Nu params body))

(defn ν? [x]
  (instance? Nu x))


(defrecord Seq [elements]
  Contextual
  Symbolic
  (symbols [_] (symbols elements))
  Object
  (toString [_]
    (str "#seq" elements)))

(defn seq [xs]
  (->Seq (list xs)))

(defn seq? [x]
  (instance? Seq x))


(defrecord Conc [elements]
  Contextual
  Symbolic
  (symbols [_] (symbols elements))
  Object
  (toString [_]
    (str "#conc" elements)))

(defn conc [xs]
  (->Conc (list xs)))

(defn conc? [x]
  (instance? Conc x))

(defn elist? [x]
  (or (seq? x) (conc? x)))


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

(ps Resolved)

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
  (when (clojure.core/seq tail)
    (pp/print-length-loop [tail (clojure.core/seq tail)]
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
   (pp/write-out head)
   (if (list? tail)
     (format-pair head (elements tail))
     (do
       (.write ^Writer *out* " . ")
       (pp/write-out tail)))))

;;; Immediate

(ps Immediate)

(defmethod pp/simple-dispatch Immediate [i]
  (.write ^Writer *out* "~")
  (pp/write-out (:form i)))

;;; Application

(ps Application)

(defmethod pp/simple-dispatch Application [{:keys [head tail]}]
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

;;; Externs

(defmethod print-method Extern [{:keys [name]} ^Writer w]
  (.write w "#F[")
  (.write w name)
  (.write w "]"))

(defmethod pp/simple-dispatch Extern [{:keys [name]}]
  (pp/pprint-logical-block
   :prefix "#F[" :suffix "]"
   (pp/write-out name)))

;;; Nu

(ps Nu)

(defmethod pp/simple-dispatch Nu [{:keys [params body]}]
  (pp/pprint-logical-block
   :prefix ")" :suffix ")"
   (pp/write-out (symbol "#ν"))
   (format-pair (symbol "#μ") [params body])))


;;; seq & conc

(defmethod print-method Seq [{:keys [elements]} ^Writer w]
  (.write w "#seq")
  (print-method elements w))

(defmethod pp/simple-dispatch Seq [{:keys [elements]}]
  (pp/write-out "#seq")
  (pp/simple-dispatch elements))


(defmethod print-method Conc [{:keys [elements]} ^Writer w]
  (.write w "#conc")
  (print-method elements w))

(defmethod pp/simple-dispatch Conc [{:keys [elements]}]
  (pp/write-out "#conc")
  (pp/simple-dispatch elements))


;;; Emission

(defmethod print-method Emission [{:keys [kvs]} ^Writer w]
  (.write w "#E")
  (print-method kvs w))

(defmethod pp/simple-dispatch Emission [{:keys [kvs]}]
  (pp/write-out (symbol "#E"))
  (pp/simple-dispatch kvs))

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

  Resolved
  (insp [{:keys [sym form]} w level]
    (spacer w level)
    (.write w "R[")
    (.write w (str sym))
    (.write w "]\n")
    (insp form w (inc level)))

  Application
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "A\n")
    (insp (:head form) w (inc level))
    (insp (:tail form) w (inc level)))

  clojure.lang.PersistentArrayMap
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "M\n")
    (dorun (map #(insp % w (inc level)) form)))

  clojure.lang.MapEntry
  (insp [[k v] ^Writer w level]
    (insp k w level)
    (spacer w level)
    (.write w "=>\n")
    (insp v w level)
    (spacer w level)
    (.write w "-\n"))

  clojure.lang.PersistentVector
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "L\n")
    (dorun (map #(insp % w (inc level)) form)))

  Extern
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "F[")
    (.write w (:name form))
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

  Nu
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "ν\n")
    (insp (:params form) w (inc level))
    (insp (:body form) w (inc level)))

  Seq
  (insp [{:keys [elements]} ^Writer w level]
    (spacer w level)
    (.write w "seq\n")
    (dorun (map #(insp % w (inc level)) elements)))

  Conc
  (insp [{:keys [elements]} ^Writer w level]
    (spacer w level)
    (.write w "conc\n")
    (dorun (map #(insp % w (inc level)) elements)))

  Emission
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "E\n")
    (loop [kvs (elements (:kvs form))]
      (when (clojure.core/seq kvs)
        (insp (first kvs) w (inc level))
        (insp (second kvs) w (inc level))
        (recur (drop 2 kvs))))))


(defn inspect [x]
  (insp x *out* 0))

;;;;; Sugar

(def type-table
  {Immediate   :I
   Pair        :P
   Symbol      :S
   Resolved    :S
   Application :A
   Extern      :F
   Mu          :μ
   Nu          :ν
   Emission    :E
   Seq         :seq
   Conc        :conc

   clojure.lang.PersistentVector   :L
   clojure.lang.PersistentArrayMap :M
   clojure.lang.PersistentHashMap  :M
   clojure.lang.PersistentHashSet  :set})

(defn type [x]
  ;; There's nothing to gain in wrapping value types.
  (get type-table (clojure.core/type x) :V))

(def xkeys
  {:return  (keyword "return")
   :error   (keyword "error")
   :unbound (keyword "unbound")
   :env     (keyword "env")})
