(ns janus.builtins
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug]
   [janus.env :as env]
   [janus.interpreter :as i]
   [janus.runtime :as rt]))

;;;;; Magic

(defmacro when-settled [app ps bindings & body]
  ;; REVIEW: Oof...
  `(loop [tail# (:tail ~app)
          ps#   ~ps]
     (if (empty? ps#)
       (let [~bindings tail#]
         ~@body)
       (if ((first ps#) tail#)
         (recur tail# (rest ps#))
         (let [tail# (i/walk tail#)]
           (if ((first ps#) tail#)
             (recur tail# (rest ps#))
             (assoc ~app :tail tail#)))))))

;; FIXME: I don't like these when- names
(defn when-arg [f]
  (fn [app]
    (when-settled app [i/evaluated?] x (f x))))

;;;;; Simple Primitive fns

(defn call-primitive-fn
  "Given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (fn [app]
    (when-settled app [i/evaluated? (partial every? i/evaluated?)]
        args
      (try
        (apply f args)
        (catch Exception e (reset! debug/*pfn {:app app :e e}) :error)))))

(defn primitive [n f]
  (ast/extern n (call-primitive-fn f)))

(defn primitives [m]
  (reduce (fn [acc [k v]] (assoc acc (ast/symbol k) (primitive k v))) {} m))

(defn nth* [c i]
  (nth c (dec i)))

(defn rest* [xs]
  (into [] (rest xs)))

(defn empty?* [x]
  (boolean (empty? x)))

(defn not* [x]
  (assert (boolean? x))
  (not x))

(def fns
  (primitives
   {"+*"   +
    "**"   *
    "-*"   -
    "/*"   /
    ">*"   >
    "<*"   <
    "=*"   =
    "mod*" mod
    "not*" not*
    "str*" str

    "list?*"  ast/list?
    "map?*"   ast/map?
    "merge*"  merge
    "empty?*" empty?*

    "symbol?*" ast/symbol?

    "first*" first
    "rest*"  rest*

    "count*" count
    "nth*"   nth* ; Base 1 indexing

    "connect*" rt/connect
    }))

;;;;; Specialish forms

(def μ-ready?
  [#(i/evaluated? %)
   (fn [args] (every? #(ast/symbol? (env/peel %)) (butlast args)))])

(defn μ [app]
  (when-settled app μ-ready?
      args
    (let [id    (gensym)
          names (into [] (map env/peel) (butlast args))]
      (apply ast/μ id (conj names (env/declare (last args) id names))))))

(defn ν [app]
  (when-settled app μ-ready?
      args
    (let [params (env/peel (first args))
          body   (env/declare (last args) :ν [params])]
      ;; REVIEW: νs evaluate their bodies. I think that's the right thing.
      (ast/ν params (ast/immediate body)))))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (ast/emission
   (ast/list (map (fn [[k v]] (ast/list [(ast/immediate k) v]))
                  (partition 2 kvs)))))

(defn select [app]
  (when-settled app [i/evaluated? #(i/evaluated? (first %))]
      [p t f]
    (assert (boolean? p) (str "Non boolean passed to select: " p))
    (if p t f)))

(defn macros [m]
  (reduce (fn [acc [k f]]
            (assoc acc (ast/symbol k) (ast/extern k f))) {} m))

(def special
  "Things that would traditionally be special forms."
  (macros
   {"μ"      μ
    "ν"      ν
    "select" select
    "emit"   (when-arg emit)

    "seq*"  (when-arg ast/seq)
    "conc*" (when-arg ast/conc)

    ;; "first*" first*
    ;; "rest*"  rest*
    }))

;;;;; The Ur context from which all programs derive.
;;
;; I don't really like this being so ad hoc. There will have to be a takeover
;; moment when the intended long term context and history system is finally
;; built. That is to say there will be a shock in the history where we suddenly
;; have no past, no origin. Why is bootstrapping so singular like that?

(def base-env
  (reduce (fn [e [k v]] (env/bind* e k v)) env/empty-ns (merge special fns)))
