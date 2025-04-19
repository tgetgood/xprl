(ns janus.b3
  (:refer-clojure :exclude [reduce eval apply extend resolve run! reduced?])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [clojure.string :as str]
   [janus.ast :as ast]
   [janus.reader :as r]))

(defprotocol Reduce
  (reduce ^{:style/indent 2} [x dyn ccs]))

(defprotocol Eval
  (eval [x dyn ccs]))

(defprotocol Apply
  (apply [head tail dyn ccs]))

(defprotocol ChannelMap
  (lookup [this channel]))

(defprotocol Channel
  (send! [this message cb]))

;;;;; Accessors

(defn form [x] (:form x))

(defn head [x] (:head x))
(defn tail [x] (:tail x))

(defn id [x] (:name x))
(defn params [x] (:params x))
(defn body [x] (:body x))

;;;;; Runtime

(def stack (ConcurrentLinkedDeque.))

;; A task cannot be scheduled until it is ready to run.
;;
;; This is a black box: we throw the task over the fence and are assured that it
;; will eventually run, but we have no indication of when.
(defn schedule [task]
  (.add stack task))

(defn run-task [[f arg]]
  ;; (event! :task {:f f :arg arg})
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
  {:return (ast/keyword "return")
   :error  (ast/keyword "error")
   :env    (ast/keyword "env")})

(defn with-return [ccs cont]
  (assoc ccs (xkeys :return) cont))

(defn return {:style/indent 1} [ccs v]
  #_(send! (lookup ccs (xkeys :return)) v))

(defn error [ccs e]
  (schedule [(get ccs (xkeys :error)) e]))

(defrecord Stream [cache offset ch])


(defn call [f args dyn ccs]
  (try
    (return ccs (clojure.core/apply (:f f) args))
    (catch Throwable e (error ccs {:msg "Call error"
                                   :ex e}))))

(defn maccall [f args dyn ccs]
  ((:f f) args dyn ccs))

;;;;; Env

(defn empty-env []
  {})

;;;;; Interpreter

(defn reduce-coll [acc xs dyn ccs]
  (if (empty? xs)
    (return ccs acc)
    (reduce (first xs) dyn
      (with-return ccs
        (fn [x]
          (reduce-coll (conj acc x) (rest xs) dyn ccs))))))

(defn ev-chain [v dyn d ccs]
  (if (zero? d)
    (return ccs v)
    (eval v dyn (with-return ccs #(ev-chain % dyn (dec d) ccs)))))

(extend-protocol Reduce
  Object
  (reduce [x _ ccs]
    (return ccs x))

  clojure.lang.PersistentVector
  (reduce [x dyn ccs]
    (reduce-coll [] x dyn ccs))

  janus.ast.Immediate
  (reduce [x dyn ccs]
    (eval (form x) dyn ccs))

  janus.ast.Mu
  (reduce [x dyn ccs]
    (reduce (body x) dyn (with-return (capture-ccs)
                           (fn [body]
                             (return ccs (ast/μ (id x) (params x) body))))))

  janus.ast.Delay
  (reduce [x dyn ccs]
    (if-let [r (resolve [(:sym x) (:ref x)] dyn)]
      ;; (println x dyn)
      (ev-chain r dyn (dec (:depth x)) ccs)
      (return ccs x)))

  janus.ast.Application
  (reduce [x dyn ccs]
    (reduce (head x) dyn (with-return ccs #(apply % (tail x) dyn ccs)))))

(extend-protocol Eval
  Object
  (eval [x _ ccs]
    (return ccs x))

  clojure.lang.PersistentVector ; (I (L x y ...)) => (L (I x) (I y) ...)
  (eval [xs dyn ccs]
    (reduce (into [] (map ast/immediate) xs) dyn ccs))

  janus.ast.Delay
  (eval [x dyn ccs]
    (return ccs (ast/delay (:sym x) (:ref x) (inc (:depth x)))))

  janus.ast.Pair ; (I (P x y)) => (A (I x) y)
  (eval [x dyn ccs]
    (eval (head x) dyn (with-return ccs #(apply % (tail x) dyn ccs))))

  janus.ast.Immediate
  (eval [x dyn ccs]
    (eval (form x) dyn (with-return ccs #(eval % dyn ccs))))

  janus.ast.Application ; (I (A x y)) must be treated in applicative order.
  (eval [x dyn ccs]
    (reduce x dyn (with-return ccs #(eval % dyn ccs))))

  janus.ast.Symbol
  (eval [s dyn ccs]
    (resolve s dyn (with-return ccs #(reduce % (empty-env) ccs)))))

(extend-protocol Apply
  janus.ast.Application ; (A (A x y) z) proceeds from inside out:
  (apply [head tail dyn ccs]
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs))))

  janus.ast.Delay
  (apply [head tail dyn ccs]
    (return ccs (ast/application head tail)))

  janus.ast.PrimitiveMacro
  (apply [mac args dyn ccs] ; macros receive unevaluated arguments and context
    (maccall mac args dyn ccs))

  janus.ast.PrimitiveFunction
  (apply [f args dyn ccs] ; reduce the arguments before calling the primitive
    (reduce args dyn (with-return ccs #(call f % dyn ccs))))

  janus.ast.Mu
  (apply [μ args dyn ccs]
    (reduce args dyn
            (with-return ccs
              (fn [args]
                (reduce (body μ) (extend dyn [(params μ) (id μ)] args) ccs))))))
