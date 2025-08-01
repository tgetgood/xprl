(ns janus.builtins
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug]
   [janus.env :as env]
   [janus.interpreter :as i]
   [janus.runtime :as rt]))

;;;;; Magic

(defn ready-go
  {:style/indent [1]}
  [ready? go]
  (fn [{:keys [tail] :as app}]
    (let [tail (if (ready? tail) tail (i/walk tail))]
      (if (ready? tail)
        (go tail)
        (assoc app :tail tail)))))

;;;;; Simple Primitive fns

(defn call-primitive-fn
  "Given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (ready-go #(every? i/evaluated? %)
    (fn [args]
      (try
        (apply f args)
        (catch Exception e
          (reset! debug/*pfn {:f f :args args :e e})
          (ast/inspect (ast/application f args))
          (println e)
          :error)))))

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

(defn μ-ready? [args]
  (every? ast/symbol? (butlast args)))

(def μ
  (ready-go μ-ready?
    (fn [args]
      (let [names (mapv ast/unresolve (butlast args))]
        (apply ast/μ (conj names (env/unpin (last args) (into #{} names))))))))

(def ν
  (ready-go μ-ready?
    (fn [[params body]]
      ;; REVIEW: νs evaluate their bodies. I think that's the right thing.
      (ast/ν params (ast/immediate (env/unpin body #{params}))))))

(defn emit [{kvs :tail :as app}]
  (assert (even? (count kvs)))
  (ast/emission
   (ast/list (map (fn [[k v]] (ast/list [(ast/immediate k) v]))
                  (partition 2 kvs)))))

(defn select [{args :tail :as app}]
  (let [[p t f] args
        p       (if (i/evaluated? p) p (i/walk p))]
    (if (i/evaluated? p)
      (do
        (assert (boolean? p) (str "Non boolean passed to select: " p))
        ;; If p is a bool, don't walk the dead branch: it might not be safe to
        ;; do so, e.g. (select ~(empty? xs) [] ~(first xs))
        (if p t f))
      ;; If p is not a bool, it ~should~ be safe to walk both `t` & `f`...
      (assoc app :tail [p (i/walk t) (i/walk f)]))))

(defn macros [m]
  (reduce (fn [acc [k f]]
            (assoc acc (ast/symbol k) (ast/extern k f))) {} m))

(def special
  "Things that would traditionally be special forms."
  (macros
   {"μ"      μ
    "ν"      ν
    "select" select
    "emit"   emit

    ;; "seq*"  (when-arg ast/seq)
    ;; "conc*" (when-arg ast/conc)

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
  (reduce (fn [e [k v]] (env/ns-intern e k v)) env/empty-ns (merge special fns)))
