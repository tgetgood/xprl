(ns janus.builtins
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug]
   [janus.env :as env]
   [janus.interpreter :as i]
   [janus.runtime :as rt]))

;;;;; Magic

(defn evaluated? [x]
  (cond
    (ast/immediate? x)   false
    (ast/application? x) false
    (env/ctx? x)         false
    true                 true))

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
    (when-settled app [evaluated?] x (f x))))

;;;;; Simple Primitive fns

(defn call-primitive-fn
  "Given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (fn [{:keys [head tail] :as app}]
    (let [p #(and (ast/list? %) (every? evaluated? %))
          args (if (p tail) tail (i/walk tail))
          app (assoc app :tail args)]
      (if (p args)
        (try
          (apply f args)
          (catch Exception e
            (reset! debug/*pfn {:app app :e e})
            (println "\nError\n\n" @debug/*pfn)
            :error))
        app))))

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

(defn update-last [coll f & args]
  (apply update coll (dec (count coll)) f args))

(def μ-ready?
  [env/context-free? #_evaluated?
   (fn [args] (every? ast/symbol? (butlast args)))])

(defn μ [{{args :form env :env} :tail :as app}]
  (if (and (ast/list? args) (every? ast/symbol? (butlast args)))
    (let [env (env/declare (env/fill-slots env i/*env*) (butlast args))]
      (apply ast/μ (update-last args env/pin env)))
    app))

(defn ν [app]
  (when-settled app μ-ready?
      [params body]
    ;; REVIEW: νs evaluate their bodies. I think that's the right thing.
    (i/with-decls [params]
      (ast/ν params (i/freeze-env (ast/immediate body))))))

(defn emit [{:keys [tail] :as app}]
  (if (and (env/ctx? tail) (ast/list? (:form tail)))
    (let [{kvs :form env :env} tail
          env (env/fill-slots env i/*env*)]
      (assert (even? (count kvs)))
      (ast/emission
       (ast/list (map (fn [[k v]] (ast/list [(env/pin (ast/immediate k) env)
                                             (env/pin v env)]))
                      (partition 2 kvs)))))
    app))

(defn select [app]
  (when-settled app [evaluated? #(evaluated? (first %))]
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
    "emit"   emit

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
  (reduce (fn [e [k v]] (env/ns-intern e k v)) env/empty-ns (merge special fns)))
