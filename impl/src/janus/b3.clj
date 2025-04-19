(ns janus.b3
  (:refer-clojure :exclude [reduce eval apply extend resolve run! reduced?])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [clojure.string :as str]
   [janus.ast :as ast]
   [janus.reader :as r]))

(def verbose true)

(defmacro trace! [f n args]
  (when verbose
    `(println ~(name f) ~(name n) ~args)))

(defprotocol Reduce
  (reduce [x dyn ccs]))

(defprotocol Eval
  (eval [x dyn ccs]))

(defprotocol Apply
  (apply [head tail dyn ccs]))

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
  ;; (println "adding: " task)
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
    ;; (println "running: " task)
    (run-task task)
    (recur)))

(def xkeys
  {:return  (ast/keyword "return")
   :error   (ast/keyword "error")
   :unbound (ast/keyword "unbound")
   :env     (ast/keyword "env")})

(defn captured? [m]
  (true? (::captured? m)))

(defn capture-ccs []
  {::captured? true})

(defn empty-ccs []
  {})

(defn send! [ch msg]
  ;; FIXME: oversimplification using continuations as channels.
  (ch msg))

(defn emission! [[chname msg] ccs]
  (let [ch (get ccs chname)]
    (if ch
      (send! ch msg)
      (let [ch (get ccs (xkeys :unbound))]
        (if ch
          (send! ch [chname msg])
          (throw (RuntimeException.
                  (str "Can't deliver message: " msg " to " chname))))))))

(defn with-return [ccs cont]
  (assoc ccs (xkeys :return) cont))

(defn return {:style/indent 1} [ccs v]
  (send! (get ccs (xkeys :return)) v))

(defn error [ccs e]
  (send! (get ccs (xkeys :error)) e))

(defrecord Stream [cache offset ch])

(defn stream? [x]
  (instance? Stream x))

;;;;; Primitives

(defn primitive-call [f args dyn ccs]
  ((:f f) f args dyn ccs))

;;;;; Env

(defn empty-env []
  {})

(defn unbound-error [s dyn]
  ;; FIXME: We need this fallthrough for delays, but we really should be
  ;; refusing to compile on this error.
  (trace! "Unbound symbol error: " s (meta s)))

(defn delayed? [v]
  (symbol? v))

(defn dyn-bound? [s dyn]
  (contains? dyn s))

(defn dyn-lookup [s dyn]
  (let [v (get dyn s)]
    (if-let [m (meta v)]
      (case (:type m)
        :param (ast/delay s v 1)
        :recur (ast/recursion v))
      v)))

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
    (trace! :reduce :Obj x)
    (return ccs x))

  clojure.lang.PersistentVector
  (reduce [x dyn ccs]
    (trace! :reduce :L x)
    (reduce-coll [] x dyn ccs))

  janus.ast.Immediate
  (reduce [x dyn ccs]
    (trace! :reduce :I x)
    (eval (form x) dyn ccs))

  janus.ast.Mu
  (reduce [x dyn ccs]
    (trace! :reduce :μ x)
    (reduce (body x) dyn (with-return (capture-ccs)
                           (fn [body]
                             (return ccs (ast/μ (id x) (params x) body))))))

  janus.ast.Delay
  (reduce [x dyn ccs]
    (trace! :reduce :D [x (resolve [(:sym x) (:ref x)] dyn)])
    (if-let [r (resolve [(:sym x) (:ref x)] dyn)]
      ;; (println x dyn)
      (ev-chain r dyn (dec (:depth x)) ccs)
      (return ccs x)))

  janus.ast.DelayedApplication
  (reduce [x dyn ccs]
    (reduce (head x) dyn
            (with-return ccs
              (fn [head]
                (apply head (:tail x) dyn
                       (with-return ccs
                         #(ev-chain % dyn (dec (:depth x)) ccs)))))))

  janus.ast.Application
  (reduce [x dyn ccs]
    (trace! :reduce :A x)
    (reduce (head x) dyn (with-return ccs #(apply % (tail x) dyn ccs)))))

(extend-protocol Eval
  Object
  (eval [x _ ccs]
    (trace! :eval :Obj x)
    (return ccs x))

  clojure.lang.PersistentVector ; (I (L x y ...)) => (L (I x) (I y) ...)
  (eval [xs dyn ccs]
    (trace! :eval :L xs)
    (reduce (into [] (map ast/immediate) xs) dyn ccs))

  janus.ast.Delay
  (eval [x dyn ccs]
    (trace! :eval :D x)
    (return ccs (ast/delay (:sym x) (:ref x) (inc (:depth x)))))

  janus.ast.DelayedApplication
  (eval [x dyn ccs]
    (trace! :eval :DA x)
    (return ccs (ast/delayedapplication (:head x) (:tail x) (inc (:depth x)))))

  janus.ast.Pair ; (I (P x y)) => (A (I x) y)
  (eval [x dyn ccs]
    (trace! :eval :P x)
    (eval (head x) dyn (with-return ccs #(apply % (tail x) dyn ccs))))

  janus.ast.Immediate
  (eval [x dyn ccs]
    (trace! :eval :I x)
    (eval (form x) dyn (with-return ccs #(eval % dyn ccs))))

  janus.ast.Application ; (I (A x y)) must be treated in applicative order.
  (eval [x dyn ccs]
    (trace! :eval :A x)
    (reduce x dyn (with-return ccs #(eval % dyn ccs))))

  janus.ast.Symbol
  (eval [s dyn ccs]
    (trace! :eval :S [s (resolve s dyn)])
    (reduce (resolve s dyn) (empty-env) ccs)))

(extend-protocol Apply
  janus.ast.Application ; (A (A x y) z) proceeds from inside out:
  (apply [head tail dyn ccs]
    (trace! :apply :A [head tail])
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs))))

  janus.ast.Delay
  (apply [head tail dyn ccs]
    (trace! :apply :D [head tail])
    (return ccs (ast/delayedapplication head tail 0)))

  janus.ast.Primitive
  (apply [x args dyn ccs] ; macros receive unevaluated arguments and context
    (trace! :apply :F [x args])
    (primitive-call x args dyn ccs))

  janus.ast.Mu
  (apply [μ args dyn ccs]
    (trace! :apply :μ [μ args])
    (reduce args dyn
            (with-return ccs
              (fn [args]
                (reduce (body μ) (extend dyn [(params μ) (id μ)] args) ccs))))))

;;;;; Builtins

(defn createμ [_ args dyn ccs]
  (let [[params body] args
        id            (gensym)]
    (reduce params dyn (with-return ccs
                         (fn [params]
                           (reduce body (extend dyn params (with-meta id
                                                             {:type :param}))
                                   (with-return (capture-ccs)
                                              (fn [body]
                                                (return ccs
                                                  (ast/μ id params body))))))))))

(defn args-reduction
  {:style/indent 3}
  [acc args dyn ccs]
  (if (seq args)
    (eval (first args) dyn
          (with-return ccs
            (fn [k]
              (reduce (second args) dyn
                      (with-return ccs
                        (fn [v]
                          (args-reduction (conj acc [k v]) (drop 2 args)
                                          dyn ccs)))))))
    (return ccs acc)))

(defn emit [_ args dyn ccs]
  (args-reduction [] args dyn
    (with-return ccs
      (fn [kvs]
        (trace! :emission "" [(captured? ccs) kvs])
        (if (captured? ccs)
          (return ccs (ast/emission kvs))
          (dorun (map #(emission! % ccs) kvs)))))))

(defn first* [args dyn ccs]
  (reduce args dyn (with-return ccs
                     (fn [[v]]
                       #_(if (stream? v)
                         (stream-first v ccs)
                         (return ccs (first v)))))))

(defn rest* [args dyn ccs]
  (reduce args dyn (with-return ccs
                     (fn [[v]]
                       #_(if (stream? v)
                         (stream-rest v ccs)
                         ;; REVIEW: Might want to cast this into a vector...
                         (return ccs (rest v)))))))

(def macros
  {"μ"      createμ
   ;;   "ν"       createν
   "emit"   emit
   ;;   "select"  select
   "first*" first*
   "rest*"  rest*})

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
   "nth*"   nth* ; Base 1 indexing
   })

(defn reduced? [x]
  (cond
    (vector? x) (every? reduced? x)
    true (not (str/starts-with? (str (type x)) "class janus.ast"))))

(defn pwrap [f]
  "Wraps a host (pure) function into the channel system as a primitive.
  Throwables are caught and sent on the `:error` channel if connected."
  (fn [p args dyn ccs]
    (reduce args dyn (with-return ccs
                       (fn [args]
                         (if (reduced? args )
                           (try
                             (let [v (clojure.core/apply f args)]
                               (return ccs v))
                             (catch Throwable e
                               (error ccs {:msg       "Error caught in pFn."
                                           :exception e})))
                           (return ccs (ast/delayedapplication p args 0))))))))

(defn tagged [f]
  (fn [[k v]]
    (let [k' (ast/symbol k)]
      [k' (f (with-meta v {:name k'}))])))

(defn tag-primitives [x]
  (into {} (map (tagged ast/->Primitive)) x))

(def base-env
  (merge
   (tag-primitives macros)
   (tag-primitives (into {} (map (fn [[k v]] [k (pwrap v)])) fns))))

;;;;; namespaces

;;;;; repl

(def srcpath "../src/")
(def bootxprl (str srcpath "boot.xprl"))

(def env (atom base-env))

(def out (atom nil))

(defn go! [f ccs]
  (schedule [(fn [_]  (eval f (empty-env) ccs)) []])
  (run!))

(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @env))
       {
        (xkeys :return)  #(do (reset! out %) (println %))
        (xkeys :env)     #(swap! env assoc (first %) (second %))
        ;; (xkeys :unbound) (fn [x] (println "Unbound!" x))
        (xkeys :error)   (fn [e] (println {:msg   "top level error"
                                           :error e}))}))

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
