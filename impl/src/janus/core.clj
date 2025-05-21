(ns janus.core
  (:refer-clojure :exclude [name test run!])
  (:import (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [janus.ast :as ast]
   [janus.env :as env :refer [name params body head tail form kvs xnth]]
   [janus.reader :as r]
   [janus.telemetry :refer [trace!]]))

(declare walk)

;;;;; Environment

;;;;; Accessors


;;;;; Application

(defn partial? [μ]
  (not (and (or (nil? (name μ)) (ast/symbol? (name μ))) (ast/symbol? (params μ)))))

(defn μ-call [μ args]
  (trace! "invoke" μ "with" args)
  (let [μ' (if (partial? μ) (walk μ) μ)]
    (if (partial? μ')
      (ast/application μ' args)
      (walk (env/μ-bind μ' args)))))

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

(defn primitive-reduced? [x]
  (and (vector? x) (every? evaluated? x)))

(defn primitive-call [head tail]
  (let [args (walk tail)]
    (trace! "pcall" head args)
    (if (primitive-reduced? args)
      (apply (:f head) args)
      (ast/application head args))))

(defn macro-call [head tail]
  ;; Even macros can't be applied if they don't have syntactic args.
  (let [args (if (vector? tail) tail (walk tail))]
    (if (vector? args)
      ((:f head) args)
      (ast/application head args))))

(defn error-call [h t]
  (throw (RuntimeException. (str h " is not applicable, but was called with " t))))

(defn apply-head [h t]
  (let [h'  (walk h)
        app (ast/application h' t)]
    (if (evaluated? h')
      (walk app)
      app)))

;;;;; Reduction

(defn walk-μ [μ]
  (let [n (name μ)]
    (ast/μ (when n (walk n)) (walk (params μ)) (walk (body μ)))))

(defn μ-reduce [μ]
  (let [μ' (if (partial? μ) (walk-μ μ) μ)]
    (if (partial? μ')
      μ'
      ;; FIXME: This ought to be handled by the env subsystem
      (let [body (env/μ-declare μ')]
        (env/set-env (assoc μ' :body (walk body)) (env/get-env body))))))

(defn emit-reduce [x]
  (ast/emission (walk (kvs x))))

(defn list-xform [f xs]
  (reduce (fn [acc i] (conj acc (f (xnth xs i)))) [] (range (count xs))))

(defn list-reduce [xs]
  (list-xform walk xs))

;;;;; Eval

(defn eval-symbol [s]
  (let [env (env/get-env s)]
    (if-let [ref (env/lookup env s)]
      (do
        (trace! "resolve" s ":" ref)
        (walk ref))
      (do
        (trace! (if (contains? (env/decls env) s) "declared" "undeclared") s)
        (ast/immediate s)))))

(defn eval-list [xs]
  (walk (list-xform ast/immediate xs)))

(defn eval-pair [p]
  (walk (ast/application (ast/immediate (head p)) (tail p))))

(defn eval-step [x]
  (let [inner (walk x)]
    (if (evaluated? inner)
      (walk (ast/immediate inner))
      (ast/immediate inner))))

;;;;; Tree walker

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

(def rules
  {[:I :S] eval-symbol ; env lookup
   [:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-step   ; eval inner form and recur
   [:I :A] eval-step   ; "
   :I      identity    ; (I V) => V. values eval to themselves

   ;; In general we cannot walk into structures. These are the exceptions.
   :L list-reduce
   :μ μ-reduce
   :E emit-reduce

   [:A :I] apply-head ; (apply x args) => (apply (walk x) args)
   [:A :A] apply-head ; iff x is unevaluated.

   ;; An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; [:A :E] apply-emit

   ;; Three kinds of operators are built in. I don't think we need any others,
   ;; but that might change.
   ;;
   ;; In fact, I'm not 100% convinced we need to distinguish primitive fns from
   ;; primitive macros as a language feature. It ought to be possible to
   ;; implement a primitive fn as a primitive macro, but it has proven mightily
   ;; inconvenient.
   [:A :M] macro-call
   [:A :F] primitive-call
   [:A :μ] μ-call
   :A      error-call})

(defn make-tree [rules]
  (reduce (fn [acc [k v]]
            (assoc-in acc (if (vector? k) (conj k :fn) [k :fn]) v))
          {} rules))

(def rule-tree (make-tree rules))

(defn step [x]
  (cond
    (instance? janus.ast.Immediate x)   (form x)
    (instance? janus.ast.Application x) (head x)
    true                                nil))

(defn walk1 [sexp]
  (let [t1 (ast-type sexp)]
    (if-let [subtree (get rule-tree t1)]
      (let [subexp (step sexp)
            t2 (ast-type subexp)]
        (if-let [subsubtree (get subtree t2)]
          [[t1 t2] (:fn subsubtree)]
          [t1 (:fn subtree)]))
      [t1 identity])))

(defn smart-call [f x]
  ;; Not actually all that smart...
  (cond
    (instance? janus.ast.Immediate x)   (f (form x))
    (instance? janus.ast.Application x) (f (head x) (tail x))
    true                                (f x)))

(defn walk [sexp]
  (let [[rule f] (walk1 sexp)
        _        (trace! "rule match:" rule sexp (env/locals (env/merge-env sexp (step sexp))))
        v        (smart-call f sexp)]
    (trace! "result:" rule "\n" sexp "\n->\n" v)
    v))

;;;;; Builtins

(defn createμ [args]
  (let [[name params body] (case (count args)
                             3 args
                             2 `[nil ~@args])]
    (walk (with-meta (ast/μ name params body) (meta args)))))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (walk
   (with-meta
     (ast/emission (into [] (map (fn [[k v]] [(ast/immediate k) v])) (partition 2 kvs)))
     (meta kvs))))

(defn select {:name "select"} [args]
  (let [[p t f] (map #(walk (xnth args %)) (range 3))]
    (cond
      (boolean? p)         (if p t f)
      (not (evaluated? p)) (ast/application (ast/macro #'select) [p t f]))))

;;;;; Boilerplate

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
   "rest*"  #(into [] (rest %)) ; We don't deal with seqs

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
  (reduce (fn [acc [sym val]] (env/ns-bind acc sym val)) (env/empty-ns)
          (concat
           (map (tagged ast/macro) macros)
           (map (tagged ast/pfn) fns))))

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
  (let [err     (fn [_] (throw (RuntimeException. (str "No such channel: " chn))))
        unbound (get ccs (xkeys :unbound) err)]
    (schedule [(get ccs chn unbound) msg])))

(defn perform-emit! [x ccs]
  (loop [kvs (kvs x)]
    (when (seq kvs)
      (let [[chn msg] (first kvs)]
        (trace! "sending on" chn ":" msg)
        (send! ccs chn msg))
      (recur (rest kvs)))))

(defn send-return! [v ccs]
  (send! ccs (xkeys :return) v))

(def connection-rules
  {:E perform-emit!})

(defn connection [x]
  (get connection-rules (ast-type x) send-return!))

(defn connect [form ccs]
  ((connection form) form ccs))

;;;;; UI

(def srcpath "../src/")
(def recxprl (str srcpath "recur.xprl"))
(def core (str srcpath "core.xprl"))
(def testxprl (str srcpath "test.xprl"))

;; (def env (atom base-env))

(defn go!
  ([env f]
   (walk (env/set-env (ast/immediate f) env)))
  ([env f ccs]
   (schedule [(fn [_] (connect (go! env f) ccs))])
   (run!)))

(defn ev [s]
  (go! @env/env (:form (r/read (r/string-reader s)))))

(defn iev [s]
  (ast/inspect (ev s)))

(defn loadfile [envatom fname]
  (let [conts {(xkeys :env)    (fn [[sym value]] (swap! envatom env/ns-bind sym value))
               ;; FIXME: This should log a warning. It's not a fatal error
               (xkeys :return) #(throw (RuntimeException. "return to top level!"))
               (xkeys :error)  (fn [x]
                                 (println "Error: " x))}]
    (loop [reader (r/file-reader fname)]
      (let [reader (r/read reader)
            form   (:form reader)]
        (if (= :eof form)
          'Done #_@envatom
          (do
            (go! @envatom form (with-return conts println))
            (recur reader)))))))

(defn reload! [fname]
  (alter-var-root #'env/env (constantly (atom base-env)))
  (loadfile env/env fname))

(defmacro inspect [n]
  `(-> @env (get-in [:names (ast/symbol ~(clojure.core/name n))]) ast/inspect))

(defn el [form name]
  (env/lookup (env/get-env form) (ast/symbol name)))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @env/env))))

(defn test []
  (let [conts {(xkeys :env) (fn [[sym value]] (swap! env/env env/ns-bind sym value))}]
    (loop [reader (r/file-reader testxprl)]
      (let [reader (r/read reader)
            form1  (:form reader)
            reader (r/read reader)
            form2  (:form reader)]
        (if (= :eof form1)
          'Done #_@envatom
          (do
            (println "Evaluating: " form1)
            (println "---")
            (print "result: ")
            (go! @env/env form1 (with-return conts println))
            (print "expected: " )
            (go! @env/env form2 (with-return conts println))
            (recur reader)))))))
