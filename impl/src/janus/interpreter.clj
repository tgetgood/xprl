(ns janus.interpreter
  (:refer-clojure :exclude [resolve run! binding declare])
  (:import (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [janus.ast :as ast]
   [janus.reader :as r]))

(def verbose false)

(defmacro trace! [& args]
  (when verbose
    `(println ~@args)))

(clojure.core/declare walk)

;;;;; Environment

(defn empty-ns []
  {:names {} :declared #{}})

(defn resolve [env s]
  (trace! "resolve" s env)
  (if-let [ref (get-in env [:names s])]
    (walk :reduce (empty-ns) ref) ; ref must be a μ so will have it's own env.
    (if (contains? (:declared env) s)
      (ast/immediate s)
      (throw (RuntimeException. (str "Unbound symbol: " s))))))

(defn declare [env s]
  (update env :declared conj s))

(defn bind [env s val]
  (let [env' (assoc-in env [:names s] val)]
    (if (contains? (:declared env) s)
      (update env' :declared disj s)
      env')))

(defn fill-declarations [calling-env μ-env]
  (reduce (fn [env s] (if (contains? (:names calling-env) s)
                        (bind env s (get-in calling-env [:names s]))
                        env))
          μ-env
          (:declarations μ-env)))

(defn μ-declare [env μ]
  (let [env' (declare (fill-declarations env (:env μ)) (:params μ))]
    (if (:name μ)
      (declare env' (:name μ))
      env')))

(defn μ-bind [env μ args]
  (let [env' (bind (fill-declarations env (:env μ)) (:params μ) args )]
    (if (:name μ)
      (bind env' (:name μ) μ)
      env')))

;;;;; application

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

(defn primitive-reduced? [x]
  (and (vector? x) (every? evaluated? x)))

(defn primitive-call [env [head tail]]
  (let [args (walk :reduce env tail)]
    (trace! "pcall" head args)
    (if (primitive-reduced? args)
      (apply (:f head) args)
      (ast/application head args))))

(defn macro-call [env [head tail]]
  ((:f head) env tail))

(defn μ-invoke [env [μ args]]
  (trace! "invoke" μ "with" args)
  (let [args' (walk :reduce env args)]
    (if (and (ast/symbol? (:params μ)) (evaluated? args'))
      (walk :reduce (μ-bind env μ args) (:body μ))
      (ast/application μ args'))))

(defn application [_ [head tail]]
  (ast/application head tail))

;;;;; reduction

(defn μ-reduce [env μ]
  (if (ast/symbol? (:params μ))
    (assoc μ :body (walk :reduce (μ-declare env μ) (:body μ)))
    (let [μ' (assoc μ
                    :params (walk :reduce env (:params μ))
                    :body   (walk :reduce env (:body μ) true))]
      (if (ast/symbol? (:params μ'))
        (walk :reduce env μ')
        μ'))))

(defn emit-walk [env acc kvs]
  (if (seq kvs)
    (let [[k v] (first kvs)
          k' (walk :reduce env (ast/immediate k))
          v' (walk :reduce env v)]
      (trace! "emit-walk" k "->" k' ":" v "->" v')
      (recur env (conj acc [k' v']) (rest kvs)))
    acc))

(defn reduce-emission [env {:keys [kvs] :as e}]
  (assoc e :kvs (emit-walk env [] kvs)))

;;;;; Interpreter

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
  {:reduce
   {:recur-check not=

    :rules {:I (fn [env {:keys [form]}] (walk :eval env (walk :reduce env form)))
            :A (fn [env {:keys [head tail]}]
                 (walk :apply env [(walk :reduce env head) tail]))
            :μ μ-reduce
            :L (fn [env xs] (into [] (map (partial walk :reduce env)) xs))
            :E reduce-emission

            :default (fn [_ form] form)}}

   :eval
   {:rules { ;; (I (L x y ...)) => (L (I x) (I y) ...)
            :L (fn [env xs] (walk :reduce env (into [] (map ast/immediate) xs)))
            ;; (I (P x y)) => (A (I x) y)
            :P (fn [env {:keys [head tail]}]
                 (walk :reduce env (ast/application (ast/immediate head) tail)))
            :S resolve

            :default (fn [env form]
                       (if (evaluated? form)
                         form
                         (ast/immediate form)))}}

   :apply
   {:dispatch (fn [[head tail]] head)
    :rules {:μ μ-invoke
            :F primitive-call
            :M macro-call
            ;; These (E, I & A) are here as rules since fallthrough allows
            ;; anything to be in the head of a pair without triggering an error
            ;; (the computation just stalls on an A node.
            :E application
            :I application
            :A application

            ;; This would be where we add programmer defined extensions...
            :default (fn [_ [h _]]
                       (throw (RuntimeException. (str h " is not applicable."))))}}})


(defn walk
  ([state env form] (walk state env form false))
  ([state env form early-stop?]
   (let [t (ast-type ((get-in rules [state :dispatch] identity) form))]
     (trace! (name state) "rule" t form)
     (let [r (:rules (get rules state))
           v ((get r t (get r :default)) env form)]
       (trace! (name state) "post" (ast-type v) v)
       (if (and ((get-in rules [state :recur-check] (constantly false)) form v)
                ;; HACK: Is there no better way to do this?
                (not early-stop?))
         (walk state env v false)
         v)))))

;;;;; Builtins

(defn createμ [env args]
  (let [[name params body] (if (= 3 (count args)) args `[nil ~@args])]
    (with-meta (ast/μ  name params body env)
      (meta body))))

(defn emit [_ kvs]
  (assert (even? (count kvs)))
  (with-meta (ast/emission (map vec (partition 2 kvs)))
    (meta kvs)))

(defn select {:name "select"} [env [p t f]]
  (let [p (walk :reduce env p)]
    (cond
      (boolean? p)         (if p t f)
      (not (evaluated? p)) (ast/application (ast/->Macro #'select) [p t f]))))

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
  {:E perform-emit!})

(defn connection [x]
  (get connection-rules (ast-type x) send-return!))

(defn connect [form ccs]
  ((connection form) form ccs))

;;;;; UI

(def srcpath "../src/")
(def recxprl (str srcpath "recur.xprl"))
(def bootxprl (str srcpath "boot.xprl"))
(def testxprl (str srcpath "test.xprl"))

(def env (atom (assoc (empty-ns) :names base-env)))

(defn go!
  ([env f]
   (walk :reduce env (ast/immediate f)))
  ([env f ccs]
   (schedule [(fn [_] (connect (go! env f) ccs))])
   (run!)))

(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @env))))

(defn iev [s]
  (ast/inspect (ev s)))

(defn loadfile [envatom fname]
  (let [conts {(xkeys :env)    #(swap! envatom assoc-in [:names (first %)] (second %))
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
            (go! @env form (with-return conts println))
            (recur reader)))))))

(defmacro inspect [n]
  `(-> @env (get-in [:names (ast/symbol ~(name n))]) ast/inspect))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @env))))
