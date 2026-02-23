(ns xprl.ast
  (:refer-clojure
   :exclude
   [symbol
    symbol?
    keyword
    keyword?
    empty
    list
    list?
    ref
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

;;;;; AST

(def ^:dynamic *verbose* false)

(defrecord Dot []
  Object
  (toString [_] "."))

(defonce dot (->Dot))

(defn dot? [x]
  (instance? Dot x))

(defn invalid-name? [s]
  (boolean (re-find #"^\.|\.\.|\.$" s)))

(defn split-symbolic [s t]
  (cond
    (= s ".")             dot
    (invalid-name? s)     (assert false (str s " is not a valid name for " t))
    (str/includes? s ".") (str/split s #"\.")
    true                  [s]))

(defmacro named [t s]
  `(let [names# (split-symbolic ~s ~t)]
     (if (= names# dot)
       dot
       (new ~t names#))))

;; Keywords are values, which is to say they're context free
(defrecord Keyword [names]
  Object
  (toString [_]
    (transduce (interpose ".") str ":" names)))

(defn keyword? [k]
  (instance? Keyword k))

(def keyword
  (memoize (fn [s] (named Keyword s))))

(defrecord Symbol [names]
  Object
  (toString [_]
    (transduce (interpose ".") str "" names)))

(def symbol-cache
  (memoize (fn [s] (named Symbol s))))

(defn symbol? [s]
  (instance? Symbol s))

(defrecord Ref [sym binding]
  Object
  (toString [_]
    (str sym "^" (when *verbose* (str "<" binding ">")))))

(defn ref? [x]
  (instance? Ref x))

(defn ref [sym local]
  (assert (symbol? sym))
  (->Ref sym local))

(defn symbolic? [x]
  (or (symbol? x) (ref? x)))

(defn symbol [x]
  (cond
    (string? x) (symbol-cache x)
    (symbol? x) x
    (ref? x)    (:sym x)
    true        (throw (RuntimeException.
                        (str "Can't create symbol from " (type x))))))

(defn unique-symbol [x]
  (symbol (str (gensym (symbol x)))))

(defn elements [l]
  l)

(defn list [xs]
  (into [] xs))

(defn list? [x]
  (vector?  x))

;; Records are IMaps which is a royal pain in the ass.
(defn map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn set? [x]
  (instance? clojure.lang.PersistentHashSet x))

;; Clojure's default predicates are too inclusive for our purposes.
(defn coll? [x]
  (or (list? x) (map? x) (set? x)))

(defrecord Pair [head tail]
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
  Object
  (toString [_]
    (str "~" form)))

(defn immediate [form]
  (->Immediate form))

(defn immediate? [x]
  (instance? Immediate x))


(defrecord Application [env head tail]
  Object
  (toString [_]
    (str "#" (str (pair head tail)))
    #_(if (and (= "#F[nth*]" (str head)) (int? (last tail)))
      (str "|" (first tail) "|_" (last tail))
      (str "#" (str (pair head tail))))))

(defn application
  ([head tail] (application {} head tail))
  ([env head tail] (->Application env head tail)))

(defn application? [x]
  (instance? Application x))


(defrecord Mu [name params body]
  Object
  (toString [_]
    (str "(#μ " params " " body ")")))

(defn μ
  ([params body] (μ nil params body))
  ([name params body]
   (assert (or nil? name) (symbol? name))
   (assert (symbol? params))
   (->Mu name params body)))

(defn μ? [x]
  (instance? Mu x))


(defn fname [f]
  (let [s (:name (meta f))]
    (cond
      (clojure.core/symbol? s) (name s)
      (string? s)              s
      true                     (str f))))


(defrecord Extern [name fn]
  Object
  (toString [_]
    (str "#F[" name "]")))

(defn extern [name fn]
  (->Extern name fn))

(defn external? [x]
  (instance? Extern x))

(defrecord Context [chs form]
  Object
  (toString [_]
    (str "#Ctx" form)))

(defn ctx [channels form]
  (->Context channels form))

(defn ctx? [x]
  (instance? Context x))


(defrecord Emission [kvs]
  Object
  (toString [_]
    (str "#E" kvs)))

(defn emission [kvs]
  (->Emission kvs))

(defn emission? [x]
  (instance? Emission x))


;; Represents passing a message to something which can receive a message.
;; REVIEW: Send, Emit, Receive, ...?
(defrecord Call [inst args]
  Object
  (toString [_]
    (str [inst args])))

(defn call [inst msgs]
  (->Call inst msgs))

(defn call? [x]
  (instance? Call x))


(defrecord LooseEnd [sym id]
  Object
  (toString [_]
    (str "->|" sym "(" id ")")))

(defn input? [x]
  (instance? LooseEnd x))

(defn input [sym id]
  (->LooseEnd sym id))

;;;;; Pretty Printing
;;
;; This comprises so much messy logic that I'm going to dump it all here to keep
;; it out of the way.

;; Boilerplate reducer.
(defmacro ps [type]
  `(defmethod print-method ~type [o# ^Writer w#]
     (.write w# (str o#))))

(defmacro pps [type]
  `(defmethod pp/simple-dispatch ~type [o#]
     (pp/write-out (clojure.core/symbol (str o#)))))

;;; Symbol

(ps Symbol)
(pps Symbol)

(ps Ref)
(pps Ref)

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

;;; Externs

(defmethod print-method Extern [{:keys [name]} ^Writer w]
  (.write w "#F[")
  (.write w (str name))
  (.write w "]"))

(defmethod pp/simple-dispatch Extern [{:keys [name]}]
  (pp/pprint-logical-block
   :prefix "#F[" :suffix "]"
   (pp/write-out name)))

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

  Ref
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "R[")
    (.write w (str (:sym form)))
    (.write w "]\n")
    (when *verbose*
      (insp (:binding form) w (inc level))))

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
    (.write w ^String (:name form))
    (.write w "]\n"))

  Mu
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "μ\n")
    (insp (:params form) w (inc level))
    (insp (:body form) w (inc level)))

  Context
  (insp [{:keys [chs form]} ^Writer w level]
    (spacer w level)
    (.write w "Ctx")
    (when *verbose*
      (.write w "[")
      (run! #(.write w (str %)) (interpose " " (sort-by :names (keys chs))))
      (.write w "]"))
    (.write w "\n")
    (when form
      (insp form w (inc level))))

  Emission
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "E\n")
    (loop [kvs (elements (:kvs form))]
      (when (<= 2 (count kvs))
        (insp (first kvs) w (inc level))
        (insp (second kvs) w (inc level))
        (recur (drop 2 kvs))))))

(defn inspect [x]
  (insp x *out* 0))

;;;;; Sugar

(defn xkey [x]
  (keyword (name x)))

(defn incomplete? [x]
  (if (coll? x)
    (some incomplete? x)
    (or (input? x) (immediate? x) (application? x))))
