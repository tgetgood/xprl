(ns xprl.ast
  (:refer-clojure
   :exclude
   [symbol
    symbol?
    keyword
    keyword?
    coll?
    empty
    list
    list?
    bound?
    ref
    seq
    seq?
    map?
    set?
    resolve])
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str])
  (:import
   (java.io Writer)))

;;;;; AST

(def ^:dynamic *verbose* false)

(defn cname [ct type]
  (clojure.core/symbol (if ct ct (str/lower-case type))))

(defn pname [p type]
  (clojure.core/symbol (if p p (str (str/lower-case type) "?"))))

(defmacro defxprl
  {:style/indent :defn}
  [type members {:keys [str constructor predicate print pprint]}]
  `(do
     (defrecord ~type ~members
       Object
       (toString [_#]
         ~str))

     ~(when (not= :none constructor)
        `(defn ~(cname constructor type) ~members
           (new ~type ~@members)))

     (defn ~(pname predicate type) [x#]
       (instance? ~type x#))

     ~(when (not= :none print)
        (if print
          `(defmethod print-method ~type [{:keys ~members} ^Writer w#]
             (~print w#))
          `(defmethod print-method ~type [o# ^Writer w#]
             (.write w# (str o#)))))

     ~(when (not= :none pprint)
        (if pprint
          `(defmethod pp/simple-dispatch ~type [{:keys ~members}]
             ~pprint)
          `(defmethod pp/simple-dispatch ~type [o#]
             (pp/write-out (clojure.core/symbol (str o#))))))))

(defxprl Dot []
  {:str "."
   :constructor :none})

(defonce dot (->Dot))

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

(defxprl Keyword [names]
  {:str         (transduce (interpose ".") str ":" names)
   :constructor :none
   :pprint      (pp/write-out (clojure.core/keyword
                               (transduce (interpose ".") str "" names)))})

(def keyword
  (memoize (fn [s] (named Keyword s))))

(defxprl Symbol [names]
  {:str         (transduce (interpose ".") str "" names)
   :constructor :none})

(def symbol-cache
  (memoize (fn [s] (named Symbol s))))

(defxprl Ref [sym binding]
  {:str (str sym "^" (when *verbose* (str "<" binding ">")))})

(defxprl Captured [sym id]
  {:str         (str sym "->(" id ")")
   :constructor capture})

(defxprl Bound [sym id binding]
  {:str         (str sym "<" id ">")
   :constructor none})

(defn bind [{:keys [sym id]} binding]
  (->Bound sym id binding))

(defn symbolic? [x]
  (or (symbol? x) (ref? x) (captured? x) (bound? x)))

(defn symbol [x]
  (cond
    (string? x)   (symbol-cache x)
    (symbol? x)   x
    (captured? x) (:sym x)
    (bound? x)    (:sym x)
    (ref? x)      (:sym x)
    true          (throw (RuntimeException.
                        (str "Can't create symbol from " (type x))))))

(defxprl Immediate [form]
  {:str    (str "~" form)
   :pprint (do (pp/write-out "~")
               (pp/write-out form))})

(defxprl Emission [env msgs]
  {:str    (str "#E" msgs)
   :print  (fn [^Writer w]
             (.write w "#E")
             (print-method msgs w))
   :pprint (do (pp/write-out (symbol "#E"))
               (pp/simple-dispatch msgs))})

(defn elements [l]
  l)

(defn list [xs]
  (into [] xs))

(defn list? [x]
  (vector? x))

;; Records are IMaps which is a royal pain in the ass sometimes.
(defn map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn set? [x]
  (instance? clojure.lang.PersistentHashSet x))

;; Clojure's default predicates are too inclusive for our purposes.
(defn coll? [x]
  (or (list? x) (map? x) (set? x)))


(defxprl Pair [head tail]
  {:str    (str "(" (str head) " "
             (if (list? tail)
               (transduce (comp (map str) (interpose " ")) str "" (elements tail))
               (str ". " (str tail)))
             ")")
   :print  :none
   :pprint :none})

(defxprl Application [head tail]
  {:str    (str "#" (str (pair head tail)))
   :pprint (do (pp/write-out "#")
               (pp/simple-dispatch (pair head tail)))})

(defxprl Mu [id rec name param body]
  {:str         (str "(#μ " param " " body ")")
   :constructor :none
   :predicate   μ?
   :pprint      :none})

(defn μ [id rec name param body]
  (assert (symbol? param))
  (assert (or (nil? name) (symbol name)))
  (->Mu id rec name param body))

(defxprl Extern [name fn]
  {:str       (str "#F[" name "]")
   :predicate external?
   :pprint    (pp/pprint-logical-block
               :prefix "#F[" :suffix "]"
               (pp/write-out name))})

(defn call
  "Invokes primitive `f` with args `t` in `env`."
  [env f t]
  ((:fn f) env f t))

;;;;; Pretty Printing
;;
;; This comprises so much messy logic that I'm going to dump it all here to keep
;; it out of the way.

;; Pair

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

;; Mu

(defmethod pp/simple-dispatch Mu [{:keys [param body]}]
  (pp/pprint-logical-block
   :prefix "(" :suffix ")"
   (pp/write-out (symbol "#μ"))
   (format-pair (symbol "#μ") [param body])))

;;;;; Sugar

(defn xkey [x]
  (keyword (name x)))

(defn incomplete? [x]
  (if (coll? x)
    (some incomplete? x)
    (or (immediate? x) (application? x))))

(defn empty
  "Wrapper for clojure.core/empty that returns `[]` given a MapEntry."
  [x]
  (if (map-entry? x)
    []
    (clojure.core/empty x)))
