(ns janus.b2
  (:refer-clojure :exclude [reduce eval apply extend resolve run!])
  (:require
   [janus.ast :as ast]
   [janus.reader :as r]))

(defprotocol Reduce
  (reduce [x dyn]))

(defprotocol Eval
  (eval [x dyn]))

(defprotocol Apply
  (apply [head tail dyn]))

(defprotocol Exec
  (exec [x ccs]))

(defprotocol ExecApply
  (exapply [head tail ccs]))
;;;;; Accessors

(defn form [x] (:form x))

(defn head [x] (:head x))
(defn tail [x] (:tail x))

(defn μname [x] (:name x))
(defn id [x] (:id x))
(defn params [x] (:params x))
(defn body [x] (:body x))

;;;;; Env

(defn empty-env []
  {})

(defn unbound-error [s dyn]
  (throw (RuntimeException. (str "Unresolved symbol: " (str s)))))

(defn delayed? [v]
  (symbol? v))

(defn dyn-bound? [s dyn]
  (contains? dyn s))

(defn dyn-lookup [s dyn]
  (let [v (get dyn s)]
    (case (first v)
      :param (ast/delay s (second v) 1)
      :recur (ast/recursion (second v)))))

(defn lex [x]
  (-> x meta :lex (get x)))

(defn lex-bound? [s]
  (not (nil? (lex s))))

(defn lex-lookup [s]
  (lex s))

(defn resolve [s dyn]
  ;; (println s (dyn-bound? s dyn) (lex-lookup s))
  (cond
    (dyn-bound? s dyn) (dyn-lookup s dyn)
    (lex-bound? s)     (lex-lookup s)
    true               (unbound-error s dyn)))

(defn extend [dyn & kvs]
  (clojure.core/apply assoc dyn kvs))

;;;;; primitives

(defn call [f args]
  (clojure.core/apply (:f f) args))

(defn maccall [f args dyn]
  ((:f f) args dyn))

;;;;; Step compiler

(defn ev-chain [v dyn d]
  (if (zero? d)
    v
    (eval (ev-chain v dyn (dec d)) dyn)))

(extend-protocol Reduce
  Object
  (reduce [x _] x)

  janus.ast.Delay
  (reduce [x dyn]
    (let [r (get dyn (:sym x))]
      ;; (println x dyn)
      (if (and r (= (:ref x) (first r)))
        (ev-chain (second r) dyn (dec (:depth x)))
        x)))

  janus.ast.Immediate
  (reduce [x dyn]
    (eval (form x) dyn))

  clojure.lang.PersistentVector
  (reduce [xs dyn]
    (into [] (map #(reduce % dyn)) xs))

  janus.ast.Mu
  (reduce [x dyn]
    (ast/μ (:name x) (:params x) (reduce (:body x) dyn) {}))

  janus.ast.Application
  (reduce [x dyn]
    (apply (reduce (head x) dyn) (tail x) dyn)))

(extend-protocol Eval
  Object
  (eval [x _] x)

  janus.ast.Delay
  (eval [x dyn]
    (ast/delay (:sym x) (:ref x) (inc (:depth x))))

  janus.ast.Pair ; (I (P x y)) => (A (I x) y)
  (eval [x dyn]
    (apply (eval (head x) dyn) (tail x) dyn))

  janus.ast.Immediate
  (eval [x dyn]
    (eval (eval (form x) dyn) dyn))

  janus.ast.Application ; (I (A x y)) must be treated in applicative order.
  (eval [x dyn]
    (let [a (reduce x dyn)]
      (if (instance? janus.ast.Application a)
        (ast/immediate a)
        (eval a dyn))))

  clojure.lang.PersistentVector ; (I (L x y ...)) => (L (I x) (I y) ...)
  (eval [xs dyn]
    (reduce (into [] (map ast/immediate) xs) dyn))

  janus.ast.Symbol
  (eval [s dyn]
    (resolve s dyn)))

(extend-protocol Apply
  Object
  (apply [head tail dyn] ; reduce the arguments before calling the primitive
    ;; (println "A" head tail dyn)
    (ast/application (reduce head dyn) (reduce tail dyn)))

  ;; janus.ast.Application ; (A (A x y) z) proceeds from inside out:
  ;; (apply [head tail dyn]
  ;;   (apply (reduce head dyn) tail dyn))

  janus.ast.PrimitiveMacro
  (apply [mac args dyn] ; macros receive unevaluated arguments and context
    (maccall mac args dyn))

  janus.ast.Mu
  (apply [μ args dyn]
    ;; (println "μ" (params μ) (:name μ) (reduce args dyn) (body μ))
    (reduce (body μ) (extend dyn (params μ) [(μname μ) (reduce args dyn)]))))

(extend-protocol Exec
  janus.ast.Application
  (exec [x ccs]
    (exapply (head x) (tail x) ccs)))

(extend-protocol ExecApply
  janus.ast.PrimitiveFunction
  (exapply [head tail ccs]
    (clojure.core/apply (:f head) tail)))

(defn execute [x]
  (exec x {}))

;;;;; Builtins

(defn createμ [args dyn]
  (let [[params body] args

        id     (gensym)
        params (reduce params dyn)

        env (extend dyn params [:param id])
        body (reduce body env)]
    (ast/μ id params body {})))

(defn emit [args dyn]
  (let [kvs (into [] (comp (map (fn [[k v]] [(eval k dyn) (reduce v dyn)])) cat)
                  (partition 2 args))]
    (ast/emission kvs)))

(def macros
  {"μ"       createμ
;   "ν"       createν
   "emit"    emit
;   "select"  select
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

   "count*" count
   "first*" first
   "rest*"  rest
   "nth*"   nth* ; Base 1 indexing
   })

(defn tagged [f]
  (fn [[k v]]
    (let [k' (ast/symbol k)]
      [k' (f (with-meta v {:name k'}))])))

(def base-env
  (merge
   (into {} (map (tagged ast/->PrimitiveMacro)) macros)
   (into {} (map (tagged ast/->PrimitiveFunction)) fns)))

;;;;; namespaces

;;;;; repl

(def srcpath "../src/")
(def bootxprl (str srcpath "boot.xprl"))

(def env (atom base-env))

(def out (atom nil))

(defn ev [s]
  (eval (:form (r/read (r/string-reader s) @env)) {}))

(defn loadfile [envatom fname]
  (loop [reader (r/file-reader fname)]
    (let [reader (r/read reader @envatom)
          form   (:form reader)]
      (if (= :eof form)
        @envatom
        (do
          (eval form {})
          (recur reader))))))

(defmacro inspect [n]
  `(-> @env (get (ast/symbol ~(name n))) ast/inspect))
