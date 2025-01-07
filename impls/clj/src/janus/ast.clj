(ns janus.ast
  (:refer-clojure :exclude [reduced? symbol keyword keyword? destructure])
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str])
  (:import
   (java.io Writer)))

;; Boilerplate reducer.
(defmacro ps [type]
  `(do (defmethod print-method ~type [o# ^Writer w#]
         (.write w# (str o#)))))

;; TODO: pp/simple-dispatch overrides how code is displayed in the repl.
;; TODO: and what does pp/code-dispatch do? Not 100% clear

;;;;; Base types

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
    (= s ".")            dot
    (re-find #"^\.+$" s) (->Symbol [s])
    :else                (->Symbol (split-symbolic s))))

(defn keyword [s]
  (->Keyword (split-symbolic s)))

;;;;; Reader AST

(defrecord Pair [head tail]
  Object
  (toString [_]
    (str "(" (str head) " "
         (if (vector? tail)
           (transduce (comp (map str) (interpose " ")) str "" tail)
           (str ". " (str tail)))
         ")")))

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

(defrecord Immediate [form env]
  Object
  (toString [_]
    (str "~" form)))

(defn immediate
  ([form] (immediate form {}))
  ([form env]
   (with-meta (->Immediate form env)
     (meta form))))

(ps Immediate)

(defmethod pp/simple-dispatch Immediate [i]
  ;; REVIEW: Is this advisable?
  (.write ^Writer *out* "~")
  (pp/write-out (:form i)))

(defrecord Application [head tail env]
  Object
  (toString [_]
    (str "#" (str (->Pair head tail)))))

(defn application
  ([head tail] (application head tail {}))
  ([head tail env]
   (with-meta (->Application head tail env)
     (select-keys (meta head) [:file :string :line :col]))))

(ps Application)

(defmethod pp/simple-dispatch Application [{:keys [head tail]}]
  ;; REVIEW: Is this advisable?
  (.write ^Writer *out* "#")
  (pp/simple-dispatch (->Pair head tail)))

(defn ppmu [t params body]
  ;; (pp/pprint-meta p)
  (pp/pprint-logical-block
   :prefix "(" :suffix ")"
   ;; TODO: Dispatch on head of pair to format
   (pp/write-out t)
   (format-pair (symbol "μ") [params body])))

(defn mustr [params body]
  (str "(#μ " params " " body ")"))

(defrecord Mu [name params body ccs triggers]
  Object
  (toString [_]
    (str "(#μ " params " " body ")")))

(defn μ [name params body ccs triggers]
  (->Mu name params body ccs triggers))

(ps Mu)

(defmethod pp/simple-dispatch Mu [{:keys [params body]}]
  (ppmu (symbol "#μ") params body))

(defn fname [f]
  (or (:name (meta f)) (str f)))

(defrecord PrimitiveFunction [f]
  Object
  (toString [_]
    (str "#pFn[" (fname f) "]")))

(defmethod print-method PrimitiveFunction [{:keys [f]} ^Writer w]
  (.write w "#pFn[")
  (if-let [n (:name (meta f))]
    (.write w (str n))
    (print-method f w))
  (.write w "]"))

(defmethod pp/simple-dispatch PrimitiveFunction [{:keys [f]}]
  (pp/pprint-logical-block
   :prefix "#pFn[" :suffix "]"
   (if-let [name (:name (meta f))]
     (pp/write-out name)
     (pp/write-out f))))

(defrecord PrimitiveMacro [f]
  Object
  (toString [_]
    (str "#pMac[" (fname f) "]")))

(defmethod print-method PrimitiveMacro [{:keys [f]} ^Writer w]
  (.write w "#pMac[")
  (if-let [n (:name (meta f))]
    (.write w (str n))
    (print-method f w))
  (.write w "]"))

(defmethod pp/simple-dispatch PrimitiveMacro [{:keys [f]}]
  (pp/pprint-logical-block
   :prefix "#pMac[" :suffix "]"
   (if-let [name (:name (meta f))]
     (pp/write-out name)
     (pp/write-out f))))

;;;;; Destructuring

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

  clojure.lang.PersistentVector
  (binding? [x] (every? binding? x))
  (bindings [x] (into [] (comp (map bindings) cat) x))
  (destructure [x y]
    (if (or (not (sequential? y)) (not= (count x) (count y)))
      nil
      (reduce merge (map destructure x y))))

  Object
  (binding? [_] false)
  (bindings [_] [])
  (destructure [xs ys] nil))
