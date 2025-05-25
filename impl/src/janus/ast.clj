(ns janus.ast
  (:refer-clojure
   :exclude
   [reduced? symbol symbol? keyword keyword? destructure delay type list List])
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str])

  (:import
   (java.io Writer)))

;;;;; Context

(defprotocol Contextual)

(defn contextual? [x]
  (instance? janus.ast.Contextual x))

(defn ctx
  ([] {})
  ([x]
   (when (contextual? x) (:ctx x))))

(defn with-ctx [x ctx]
  (if (contextual? x)
    (assoc x :ctx ctx)
    x))

(defn with-symbols [ctx syms]
  (assoc ctx ::symbols syms))

(declare symbols)

(defn build-ctx
  ([& subforms]
   (with-symbols (ctx) (apply set/union (map symbols subforms)))))

;;;;; AST

(defn split-symbolic [s]
  (if (str/includes? s ".")
    (str/split s #"\.")
    [s]))

;; Keywords are values, which is to say they're context free
(defrecord Keyword [names]
  Object
  (toString [_]
    (transduce (interpose ".") str ":" names)))

(defn keyword? [k]
  (instance? Keyword k))

(defn keyword [s]
  (->Keyword (split-symbolic s)))


(defrecord Symbol [names ctx]
  Contextual
  Object
  (toString [_]
    (transduce (interpose ".") str "" names)))

(defonce dot (->Symbol "." (ctx)))

(defn symbol [s]
  (cond
    ;; TODO: Intern symbols
    (= s ".")            dot
    (re-find #"^\.+$" s) (->Symbol s (build-ctx))
    :else                (->Symbol (split-symbolic s) (build-ctx))))

(defn symbol? [s]
  (instance? Symbol s))

(defn symbols
  "Returns set of symbols mentioned in sexp."
  [x]
  (cond
    (symbol? x)     #{x}               ; symbols needn't track self mention.
    (contextual? x) (::symbols (ctx x))
    true            #{}))


(defrecord Pair [head tail ctx]
  Contextual
  Object
  (toString [_]
    (str "(" (str head) " "
         (if (vector? tail)
           (transduce (comp (map str) (interpose " ")) str "" tail)
           (str ". " (str tail)))
         ")")))

(defn pair [head tail]
  (->Pair head tail (build-ctx head tail)))


(defrecord Immediate [form ctx]
  Contextual
  Object
  (toString [_]
    (str "~" form)))

(defn immediate [form p]
  (->Immediate form (build-ctx form)))


(defrecord Application [head tail ctx]
  Contextual
  Object
  (toString [_]
    (str "#" (str (pair head tail)))))

(defn application [head tail]
  (->Application head tail (build-ctx head tail)))


(defrecord Mu [name params body ctx]
  Contextual
  Object
  (toString [_]
    (str "(#μ " params " " body ")")))

(defn μ [name params body]
  ;; REVIEW: Can μ be called if params *isn't* a symbol?
  ;; That depends on the interpreter, so let's not assume anything here.
  (let [syms (if (symbol? params) (conj (symbols body) params) (symbols body))
        syms (if (symbol? name) (conj syms name) syms)]
    (->Mu name params body (with-symbols (build-ctx) syms))))

(defn μ? [x]
  (instance? Mu x))


(defrecord List [elements ctx]
  Contextual
  Object
  (toString [_] (str elements)))

(defn list [xs]
  (->List xs (apply build-ctx xs)))


(defn fname [f]
  (or (:name (meta f)) (str f)))

(defrecord Primitive [f]
  Object
  (toString [_]
    (str "#F[" (fname f) "]")))

(defn pfn [f]
  (->Primitive f))


(defrecord Macro [f]
  Object
  (toString [_]
    (str "#M[" (fname f) "]")))

(defn macro [f]
  (->Macro f))


(defrecord Nu [name params body ctx]
  Contextual
  Object
  (toString [_]
    (str "(#ν " params " " body ")")))


(defrecord Emission [kvs ctx]
  Contextual
  Object
  (toString [_]
    (str "#E" kvs)))

(defn emission [kvs]
  (->Emission kvs (build-ctx kvs)))

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
  (if (vector? (:tail o))
    (doseq [x (:tail o)]
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
  (pp/simple-dispatch (->Pair head tail)))

;;; μ

(ps Mu)

(defmethod pp/simple-dispatch Mu [{:keys [params body]}]
  (pp/pprint-logical-block
   :prefix "(" :suffix ")"
   (pp/write-out (symbol "#μ"))
   (format-pair (symbol "#μ") [params body])))

;;; Primitive

(defmethod print-method Primitive [{:keys [f]} ^Writer w]
  (.write w "#F[")
  (if-let [n (:name (meta f))]
    (.write w (str n))
    (print-method f w))
  (.write w "]"))

(defmethod pp/simple-dispatch Primitive [{:keys [f]}]
  (pp/pprint-logical-block
   :prefix "#F[" :suffix "]"
   (if-let [name (:name (meta f))]
     (pp/write-out name)
     (pp/write-out f))))

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

  List
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "L\n")
    (dorun (map #(insp % w (inc level)) (:elements form))))

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
    (.write w (str (:name (meta (:f form)))))
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
    (loop [kvs (:kvs form)]
      (when (seq kvs)
        (insp (first kvs) w (inc level))
        (insp (second kvs) w (inc level))
        (recur (drop 2 kvs))))))


(defn inspect [x]
  (insp x *out* 0))

;;;;; Sugar

(def type-table
  {janus.ast.List        :L
   janus.ast.Immediate   :I
   janus.ast.Pair        :P
   janus.ast.Symbol      :S
   janus.ast.Application :A
   janus.ast.Primitive   :F
   janus.ast.Macro       :M
   janus.ast.Mu          :μ
   janus.ast.Emission    :E})

(defn type [x]
  ;; There's nothing to gain in wrapping value types.
  (get type-table (clojure.core/type x) :V))

(def xkeys
  {:return  (keyword "return")
   :error   (keyword "error")
   :unbound (keyword "unbound")
   :env     (keyword "env")})
