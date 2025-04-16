(ns janus.boot
  "Absolutely minimal interpreter with the goal of bootstrapping. Porting a
  bootstrapped artefact ~should~ take a lot less hand written asm. Also having a
  working interpreter to compare outputs will be invaluable for testing.

  It's unclear if I *can* bootstrap the runtime. Well it has to be possible, but
  can ~I~ figure it out?

  This ns is intended to be fully self contained except for the reader and ast
  structs. If not for those, this would be a completely new implementation."
  (:refer-clojure :exclude [reduce eval apply extend resolve run!])
  (:import
   (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [janus.ast :as ast]
   [janus.reader :as r]
   [taoensso.telemere :as t]))

(defn event! [type data]
  ;; FIXME: This should log different fields based on type and x.
  ;; Why is the type of x nil when x is a record from AST?
  (t/event! type
            {:level :trace
             :kind  ::trace
             :data  data}))

;;;;; Runtime
;;
;; I wanted to avoid having a runtime, but the work stack is integral to the
;; language semantics. I still hope to avoid executors and work stealing in the
;; bootstrap impl, but we'll see where pragmatism takes us.

;; Just use a mutable stack to get up and running. Plus it's threadsafe.
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

;;;;; Channels & Streams
;;
;; Getting this right is the crux

;; Should return a pair of (channel, stream)
(defn channel [])

;; special channel whose stream will only ever be called with `(last st)`, so we
;; can skip a bunch of logic and ensure that writes always happen immediately
;; and thus we can ensure read-after-write semantics.
(defn last-ch [])

;;;;; protocols

(defprotocol Reduce
  (reduce* [x dyn ccs]))

(defprotocol Eval
  (eval* [x dyn ccs]))

(defprotocol Apply
  (apply* [head tail dyn ccs]))

(defn reduce {:style/indent 2} [f dyn ccs]
  (event! :reduce {:form f :dyn dyn})
  (reduce* f dyn ccs))

(defn eval {:style/indent 2} [x dyn ccs]
  (event! :eval {:form x :dyn dyn})
  (eval* x dyn ccs))

(defn apply {:style/indent 3} [head tail dyn ccs]
  (event! :apply {:head head :tail tail :dyn dyn})
  (apply* head tail dyn ccs))

;; Constructors

(defn empty-env []
  {})

(defn empty-ccs []
  {})

;; communication

(def xkeys
  {:return (ast/keyword "return")
   :error  (ast/keyword "error")
   :env    (ast/keyword "env")})

(defn with-return [ccs cont]
  (assoc ccs (xkeys :return) cont))

(defn return {:style/indent 1} [ccs v]
  (schedule [(get ccs (xkeys :return)) v]))

(defn error [ccs e]
  (schedule [(get ccs (xkeys :error)) e]))

(defn call [f args dyn ccs]
  (try
    (event! :call {:f f :args args :dyn dyn})
    (return ccs (clojure.core/apply (:f f) args))
    (catch Throwable e (error ccs {:msg "Call error"
                                   :ex e}))))

(defn maccall [f args dyn ccs]
  (event! :call {:macro f :args args :dyn dyn})
  ((:f f) args dyn ccs))

;; Accessors

(defn form [x] (:form x))

(defn head [x] (:head x))
(defn tail [x] (:tail x))

(defn μname [x] (:name x))
(defn params [x] (:params x))
(defn body [x] (:body x))
(defn μenv [x] (:dyn x))

(defn lex [x]
  (-> x meta :lex (get x)))

;; env

(defn unbound-error [s dyn ccs]
  (error ccs {:msg "unresolved symbol" :sym s :dyn dyn}))

(defn dyn-bound? [s dyn]
  (contains? dyn s))

(defn dyn-lookup [s dyn]
  (get dyn s))

(defn lex-bound? [s]
  (not (nil? (lex s))))

(defn lex-lookup [s]
  (lex s))

(defn resolve [s dyn ccs]
  (event! :resolve {:sym s :dyn dyn :lex (lex s)})
  (cond
    ;; TODO: reactive μ
    ;; dyn-lookup will return a stream, just read from it.
    (dyn-bound? s dyn) (return ccs (dyn-lookup s dyn))
    (lex-bound? s)     (return ccs (lex-lookup s))
    true               (unbound-error s dyn ccs)))

(defn bind [s v]
  {s v})

(defn extend [dyn μ ext]
  (event! :extend {:env dyn :extention (bind (params μ) ext) :μenv (μenv μ)})
  (merge dyn
         (μenv μ)
         (bind (params μ) ext)
         ;; If μ is named, put its name in the env we call it with. This is a
         ;; bit of a kludge to get recursion without combinators or similar
         ;; complexities.
         ;; We don't have to worry about the stack because everything is
         ;; effectively called via a trampoline so nothing returns in any
         ;; meaningful sense.
         (let [n (μname μ)]
           (when (not= n ::anon)
             {n μ}))))

;;;;; Interpreter

(defn reduce-coll [acc xs dyn ccs]
  (if (empty? xs)
    (return ccs acc)
    (reduce (first xs) dyn
      (with-return ccs
        (fn [x]
          (reduce-coll (conj acc x) (rest xs) dyn ccs))))))

(extend-protocol Reduce
  Object
  (reduce* [x _ ccs] (return ccs x))

  janus.ast.Immediate
  (reduce* [x dyn ccs]
    (eval (form x) dyn ccs))

  clojure.lang.PersistentVector
  (reduce* [xs dyn ccs]
    (reduce-coll [] xs dyn ccs))

  janus.ast.Application
  (reduce* [x dyn ccs]
    (reduce (head x) dyn (with-return ccs #(apply % (tail x) dyn ccs)))))

(extend-protocol Eval
  Object
  (eval* [x dyn ccs] (return ccs x))

  janus.ast.Pair ; (I (P x y)) => (A (I x) y)
  (eval* [x dyn ccs]
    (eval (head x) dyn (with-return ccs #(apply % (tail x) dyn ccs))))

  janus.ast.Immediate
  (eval* [x dyn ccs]
    (eval (form x) dyn (with-return ccs #(eval % dyn ccs))))

  janus.ast.Application ; (I (A x y)) must be treated in applicative order.
  (eval* [x dyn ccs]
    (reduce x dyn (with-return ccs #(eval % dyn ccs))))

  clojure.lang.PersistentVector ; (I (L x y ...)) => (L (I x) (I y) ...)
  (eval* [xs dyn ccs]
    (reduce (into [] (map ast/immediate) xs) dyn ccs))

  janus.ast.Symbol
  (eval* [s dyn ccs]
    (resolve s dyn (with-return ccs #(reduce % (empty-env) ccs)))))

(extend-protocol Apply
  janus.ast.Application ; (A (A x y) z) proceeds from inside out:
  (apply* [head tail dyn ccs]
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs))))

  janus.ast.PrimitiveMacro
  (apply* [mac args dyn ccs] ; macros receive unevaluated arguments and context
    (maccall mac args dyn ccs))

  janus.ast.PrimitiveFunction
  (apply* [f args dyn ccs] ; reduce the arguments before calling the primitive
    (reduce args dyn (with-return ccs #(call f % dyn ccs))))

  janus.ast.Mu
  (apply* [μ args dyn ccs]
    ;; TODO: reactive μ
    ;; If we pass a stream in for the param of a μ and bind the respective
    ;; channel to the "compiled object μ" then application of args to a μ
    ;; reduces to simply putting a message on that channel.
    ;;
    ;; Questions:
    ;;
    ;; 1) Do we still need to track local bindings in a μ or will the nesting of
    ;; streams "just work"?
    ;;
    ;; 2) Given that a "compiled μ" will be an implicit graph of continuations,
    ;; how do we maintain reflectivity for inspection and debugging?
    ;;
    ;; 3) Will the code be renterable? When the same compiled μ is called in
    ;; different contexts, it must react in different ways (i.e. it must emit to
    ;; a different continuation bundle). How do we orchestrate that?
    ;;
    ;; 4) Is is possible to do this without an event loop based runtime? (I
    ;; don't think so, but I could be wrong).
    (reduce args dyn (with-return ccs
                       #(reduce (body μ) (extend dyn μ %) ccs)))))

;;;;; Builtins

(defn createμ [args dyn ccs]
  (cond
    ;; TODO: docstrings and metadata?
    (= 2 (count args)) (createμ [::anon (first args) (second args)] dyn ccs)
    (= 3 (count args)) (let [[name params body] args]
                         (reduce name dyn
                           (with-return ccs
                             (fn [name]
                               (reduce params dyn
                                 (with-return ccs
                                   (fn [params]
                                     (return ccs
                                       (with-meta (ast/μ name params body dyn)
                                         (meta args))))))))))))

(defn createν [args dyn ccs]
  (throw (RuntimeException. "unimplemented")))

(defn emit [args dyn ccs]
  ;; REVIEW: To be consistent with previous impls I'm *eval*ing the channel name
  ;; because we ought to be able to pass emit a name which resolves to a channel
  ;; name (I think).
  ;; But remember that xprl is not applicative, so we do NOT eval the messages.
  (eval (first args) dyn
    (with-return ccs
      (fn [ch]
        (reduce (second args) dyn
          (with-return ccs
            (fn [msg]
              ;; Bootstrap emit will necessarily deliver message in order
              ;; sequentially. That's because there are no executors or work
              ;; stacks; the `args` parameter is the work *queue*.
              ;;
              ;; N.B.: try not to depend overmuch on the order of delivery
              ;; because that will just cause more pain down the road when we
              ;; remove all guarantees of delivery order.
              (do
                (event! :emit {ch msg})
                (schedule [(get ccs ch) msg])
                (when (< 2 (count args))
                  (emit (drop 2 args) dyn ccs))))))))))

(defn select [[p t f] dyn ccs]
  (reduce p dyn (with-return ccs
                  #(cond
                     (= true %)  (reduce t dyn ccs)
                     (= false %) (reduce f dyn ccs)
                     true        (error ccs {:msg       "non-boolean in select"
                                             :predicate %})))))

(def macros
  {"μ"       createμ
   "ν"       createν
   "emit"    emit
   "select"  select})

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

(t/set-min-level! ::trace :info)

(def env (atom base-env))

(defn go! [form ccs]
  (schedule [(fn [_] (eval form (empty-env) ccs)) nil])
  (run!))

(def out (atom nil))

(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @env))
       {(xkeys :return)  #(do (reset! out %) (println %))
        (xkeys :env)     #(swap! env assoc (first %) (second %))
        (xkeys :unbound) (fn [x] (println "Unbound!" x))
        (xkeys :error)   (fn [e] (println {:msg   "top level error"
                                           :error e}))}))

(defn form-log! [level form msg]
  (t/log! {:level level
           :data  (assoc (select-keys (meta form) [:string :file :line :col])
                         :form form)}
                msg))
(defn loadfile [envatom fname]
  (let [conts {(xkeys :env)    #(swap! envatom assoc (first %) (second %))
               ;; FIXME: This should log a warning. It's not a fatal error
               (xkeys :return) #(throw (RuntimeException. "return to top level!"))
               (xkeys :error)  (fn [x]
                                 (t/log! {:id   :fileloader :level :error
                                          :data (dissoc x :msg)}
                                         (:msg x)))}]
    (loop [reader (r/file-reader fname)]
      (let [reader (r/read reader @envatom)
                    form   (:form reader)]
                (form-log! :debug form "eval form")
                (if (= :eof form)
                  @envatom
                  (do
                    (go! form (with-return conts println))
                    (recur reader)))))))

(defmacro inspect [n]
  `(-> @env (get (ast/symbol ~(name n))) ast/inspect))
