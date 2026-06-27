(ns xprl.debug
  (:require [xprl.ast :as ast]
            [xprl.env :as env])
  (:import [java.io Writer]))

(def ^:dynamic *verbose* false)

(def ^:dynamic *sample-interval*
  "Only show one trace each interval. A form of rate limiting."
  0)

(def counter (atom 0))

(defmacro trace! [& args]
  `(when *verbose*
     (println ~@args)
     (println)))

(defn provenance [x]
  (::provenance (meta x)))

(defn with-provenance [x p]
  (if (and p (instance? clojure.lang.IMeta x))
    (with-meta x (assoc (meta x) ::provenance p))
    x))

(defn tag [val rule predecessor]
  (with-provenance val {:rule rule :predecessor predecessor}))

;; storage location for errors when invoking clj externals. akin to *e
(defonce *pfn (atom nil))

;; This noticably slows down execution, so let's not leave in on by default for
;; now because, frankly, it doesn't carry its own weight yet. Maybe that will
;; change as I learn to use it.
(def ^:dynamic *execution-trace* false)

(defonce index (atom {}))
(def ^:dynamic *index-key* :master)

(defn record! [in out md]
  (when-not (or (nil? out) (= in out))
    (swap! index update *index-key* update out (fnil conj #{})
           (with-meta in md))))

(defn clear-index! []
  (reset! index {}))

(defmacro with-key [k body]
  `(binding [*index-key*       ~k
             *execution-trace* true]
     ~body))

(defn causes
  ([form] (causes *index-key* form))
  ([k form] (get-in @index [k form])))

(defn env
  "Walks an expression and builds a map of all parameter bindings. Recurs into
  the bindings themselves."
  [form]
  (cond
    (ast/immediate? form)   (env (:form form))
    (ast/application? form) (merge (env (:head form)) (env (:tail form)))
    (ast/pair? form)        (merge (env (:head form)) (env (:tail form)))
    (ast/μ? form)           (env (:body form))
    (ast/emission? form)    (env (:msgs form))
    (ast/net? form)         (env (:forms form))
    (ast/coll? form)        (reduce merge {} (map env form))
    (ast/bound? form)       (merge {form (:binding form)} (env (:binding form)))
    (ast/captured? form)    {form :unbound}
    true                    {}))

;; TODO: Now if I could only reverse these before printing, it would be a lot
;; easier to read...
(defmacro deftracefn [name args & body]
  (let [env   (first args)
        input (if (= 2 (count args)) (second args) (into [] (rest args)))]
    `(defn ~name ~args
       (let [~env (assoc ~env (ast/xkey :return)
                         (fn [v#]
                           (when *execution-trace*
                             (record! ~input v# {:op ~(keyword name)}))
                           (trace! "---" ~(str name)
                                   ;; "in"  (env ~input)
                                   "\n---\n" ~input "\n-->\n"
                                   ;; (env v#) "\n--\n"
                                   v# "\n---")
                           (let [ret# (get ~env (ast/xkey :return))]
                             (assert (fn? ret#) (str "no :return in:\n" ~env ))
                             (ret# v#))))]
         ~@body))))

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

  xprl.ast.Pair
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "P\n")
    (insp (:head form) w (inc level))
    (insp (:tail form) w (inc level)))

  xprl.ast.Immediate
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "I\n")
    (insp (:form form) w (inc level)))

  xprl.ast.Symbol
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "S[")
    (.write w (str form))
    (.write w "]\n"))

  xprl.ast.Ref
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "R[")
    (.write w (str (:sym form)))
    (.write w "]\n")
    (when *verbose*
      (insp (:binding form) w (inc level))))

  xprl.ast.Application
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

  xprl.ast.Extern
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "F[")
    (.write w ^String (:name form))
    (.write w "]\n"))

  xprl.ast.Mu
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "μ\n")
    (insp (:param form) w (inc level))
    (insp (:body form) w (inc level)))

  xprl.ast.Emission
  (insp [form ^Writer w level]
    (spacer w level)
    (.write w "E\n")
    (loop [kvs (flatten (:msgs form))]
      (when (<= 2 (count kvs))
        (insp (first kvs) w (inc level))
        (insp (second kvs) w (inc level))
        (recur (drop 2 kvs))))))

(defn inspect [x]
  (insp x *out* 0))
