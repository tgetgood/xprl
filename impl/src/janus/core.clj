(ns janus.core
  "Tree walking simplifier. Maybe backtracking is overkill."
  (:refer-clojure :exclude [resolve run!])
  (:import (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [clojure.walk :as walk]
   [janus.ast :as ast]
   [janus.reader :as r]))

(def verbose false)

(defmacro trace! [& args]
  (when verbose
    `(println ~@args)))

(declare reduce-walk)

;;;;; env

(def local 'local)

(defn local? [x]
  (= x local))

(defn top-resolve [s]
  (trace! "static resolve" s (-> s meta :lex (get s)))
  (if-let [b (-> s meta :lex (get s))]
    (with-meta b (meta s))
    (ast/immediate s)
    #_(throw (RuntimeException. (str "unresolvable symbol: " s)))))

(defn resolve [s]
  (let [b (:binding s)]
    (trace! "resolve" s b)
    (cond
      (= b ast/unbound) (top-resolve s)
      (local? b)    (ast/immediate s)
      true           b)))

(defn param-walk [params args body]
  (trace! "param" params "set to" args "in" body )
  (let [f (fn [form]
            (if (and (ast/symbol? form) (= (:names params) (:names form)))
              (ast/symbol (str form) args)
              form))]
    (walk/postwalk f body)))

(defn param-set [{:keys [params body]} args]
  (param-walk params args body))

;;;;; application

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

(defn primitive-reduced? [x]
  (and (vector? x) (every? evaluated? x)))

(defn primitive-call [[head tail]]
  (let [args (reduce-walk tail)]
    (trace! "pcall" head args)
    (if (primitive-reduced? args)
      (clojure.core/apply (:f head) args)
      (ast/application head args))))

(defn macro-call [[head tail]]
  ((:f head) tail))

(defn μ-invoke [[μ args]]
  (let [args' (reduce-walk args)]
    (if (evaluated? args')
      (param-set μ args')
      (ast/application μ args'))))

(defn application [[head tail]]
  (ast/application head tail))

;;;;; reduction

(defn μ-reduce [{:keys [params body]}]
  (if (ast/symbol? params)
    (ast/μ params (reduce-walk (param-walk params local body)))
    (ast/μ (reduce-walk params) (reduce-walk body))))

(defn emit-walk [acc kvs]
  (if (seq kvs)
    (let [[k v] (first kvs)
          k' (reduce-walk (ast/immediate k))
          v' (reduce-walk v)]
      (trace! "emit-walk" k "->" k' ":" v "->" v')
      (recur (conj acc [k' v']) (rest kvs)))
    acc))

(defn reduce-emission [{:keys [kvs]}]
  (ast/emission (emit-walk [] kvs)))

;;;;; walker

(def type-table
  {clojure.lang.PersistentVector :L
   janus.ast.Immediate           :I
   janus.ast.Pair                :P
   janus.ast.Symbol              :S
   janus.ast.Application         :A
   janus.ast.Primitive           :F
   janus.ast.Macro               :M
   janus.ast.Mu                  :μ
   janus.ast.Emission            :E})

(defn ast-type [x]
  (get type-table (type x) :V))

;; REVIEW: This is overengineering looked at simply. But the point is to force
;; out the similarity in the three "types" of walkers and hopefully unify them.
(defmacro defwalker [name rules params k arg found not-found]
  `(defn ~name ~params
     (let [t# (ast-type ~k)]
       (trace! (name '~name) "rule" t# ~arg)
       (if-let [f# (~rules t#)]
         (let [v# (f# ~arg)]
           (trace! (name '~name) "post" (ast-type v#) v#)
           (~found v#))
         ~not-found))))

(def eval-rules
  {;; (I (L x y ...)) => (L (I x) (I y) ...)
   :L (fn [xs] (into [] (map ast/immediate) xs))
   ;; (I (P x y)) => (A (I x) y)
   :P (fn [{:keys [head tail]}] (ast/application (ast/immediate head) tail))
   :S resolve})

(defwalker eval-walk eval-rules [x] x x
  identity
  ;; Put the `I` back if you can't evaluate.
  (if (evaluated? x) x (ast/immediate x)))

(def apply-rules
  {:μ μ-invoke
   :F primitive-call
   :M macro-call
   ;; These (I & A) are here as rules since fallthrough allows anything to be in
   ;; the head of a pair without triggering an error (the computation just
   ;; stalls on an A node.
   :I application
   :A application})

(defwalker apply-walk apply-rules [head tail] head [head tail]
  identity
  (throw (RuntimeException. (str "cannot apply (" (type head) ") " head ))))

(def reduce-rules
  {:I (fn [{:keys [form]}] (eval-walk (reduce-walk form)))
   :A (fn [{:keys [head tail]}] (apply-walk (reduce-walk head) tail))
   :μ μ-reduce
   :L (fn [xs] (into [] (map reduce-walk) xs))
   :E reduce-emission})

(defwalker reduce-walk reduce-rules [x] x x
  #(if (= x %) % (reduce-walk %))
  x)

;;;;; Builtins

(defn createμ [[params body]]
  (ast/μ params body))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (ast/emission (map vec (partition 2 kvs))))

(defn select {:name "select"} [[p t f]]
  (let [p (reduce-walk p)]
    (cond
      (boolean? p)         (if p (reduce-walk t) (reduce-walk f))
      (not (evaluated? p)) (ast/application (ast/->Macro #'select) [p t f]))))

(def macros
  {"μ"      createμ
   ;;   "ν"       createν
   "emit"   emit
   "select" select
   ;; "first*" first*
   ;; "rest*"  rest*
   })

(defn nth* [c i]
  (nth c (dec i)))

(def fns
  {"+*"   +
   "**"   *
   "-*"   -
   "/*"   /
   ">*"   >
   "<*"   <
   "=*"   =
   "mod*" mod

   "first*" first
   "rest*"  #(into [] (rest %))

   "count*" count
   "nth*"   nth* ; Base 1 indexing

   ;; "select" select
   })

(defn tagged [f]
  (fn [[k v]]
    (let [k' (ast/symbol k)]
      [k' (f (with-meta v {:name k'}))])))

(defn tag-primitives [x f]
  (into {} (map (tagged f)) x))

(def base-env
  (merge
   (tag-primitives macros ast/->Macro)
   (tag-primitives fns ast/->Primitive)))

;;;;; Runtime

(def stack (ConcurrentLinkedDeque.))

;; A task cannot be scheduled until it is ready to run.
;;
;; This is a black box: we throw the task over the fence and are assured that it
;; will eventually run, but we have no indication of when.
(defn schedule [task]
  (.add stack task))

(defn run-task [[f arg]]
  (f arg))

(defn next-task []
  (try
    (.pop stack)
    (catch java.util.NoSuchElementException e
      (println "---"))))

(defn run! []
  (when-let [task (next-task)]
    (run-task task)
    (recur)))

(def xkeys
  {:return  (ast/keyword "return")
   :error   (ast/keyword "error")
   :unbound (ast/keyword "unbound")
   :env     (ast/keyword "env")})

(defn with-return [ccs cont]
  (assoc ccs (xkeys :return) cont))

(defn send! [ccs chn msg]
  (let [err #(throw (RuntimeException. (str "No such channel: " chn)))
        unbound (get ccs (xkeys :unbound) err)]
    (schedule [(get ccs chn unbound) msg])))

(defn perform-emit! [{:keys [kvs]} ccs]
  (loop [kvs kvs]
    (when (seq kvs)
      (let [[chn msg] (first kvs)]
        (send! ccs chn msg))
      (recur (rest kvs)))))

(defn μ-connect [μ ccs]
  (throw (RuntimeException. "!!!!")))

(defn send-return! [v ccs]
  (send! ccs (xkeys :return) v))

(def connection-rules
  {:E perform-emit!
   :μ μ-connect})

(defn connection [x]
  (get connection-rules (ast-type x) send-return!))

(defn connect [form ccs]
  ((connection form) form ccs))

;;;;; UI

(def srcpath "../src/")
(def bootxprl (str srcpath "boot.xprl"))

(def env (atom base-env))

(defn go!
  ([f]
   (reduce-walk (ast/immediate f)))
  ([f ccs]
   (schedule [(fn [_] (connect (go! f) ccs))])
   (run!)))

(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @env))))

(defn iev [s]
  (ast/inspect (ev s)))

(defn loadfile [envatom fname]
  (let [conts {(xkeys :env)    #(swap! envatom assoc (first %) (second %))
               ;; FIXME: This should log a warning. It's not a fatal error
               (xkeys :return) #(throw (RuntimeException. "return to top level!"))
               (xkeys :error)  (fn [x]
                                 (println "Error: " x))}]
    (loop [reader (r/file-reader fname)]
      (let [reader (r/read reader @envatom)
            form   (:form reader)]
        (if (= :eof form)
          @envatom
          (do
            (go! form (with-return conts println))
            (recur reader)))))))

(defmacro inspect [n]
  `(-> @env (get (ast/symbol ~(name n))) ast/inspect))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @env))))
