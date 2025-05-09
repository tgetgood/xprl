(ns janus.interpreter
  (:refer-clojure :exclude [resolve run! binding])
  (:import (java.util.concurrent ConcurrentLinkedDeque))
  (:require
   [janus.ast :as ast]
   [janus.reader :as r]))

(def verbose true)

(defmacro trace! [& args]
  (when verbose
    `(println ~@args)))

(clojure.core/declare walk)

;;;;; Environment

(defn empty-ns []
  ;; REVIEW: One sanity check on a namespace is that the set of declared (but
  ;; not instantiated) names must be empty by the time we finish reading it in.
  {:names {} :declared #{}})

(defn ns-ref [val env]
  {:value val
   :env   env})

(defn resolve [env s]
  (if-let [ref (get-in env [:names s])]
    (do
      (trace! "resolve" s ":" (:value ref))
      (walk :reduce (:env ref) (:value ref)))
    (if (contains? (:declared env) s)
      (do
        (trace! "resolve" s ": declared, unbound")
        (ast/immediate s))
      (throw (RuntimeException. (str "Unbound symbol: " s))))))

(defn μ-declare-1 [env s]
  ;; FIXME: This `declare` cannot be used at the namespace level since, for
  ;; correctness, we need to assert that no name is overwritten. Namespaces are
  ;; append only (though each version of a namespace starts over so new versions
  ;; can redefine old names).
  ;;
  ;; μs parameters, however, can shadow symbols in the env, which is what this
  ;; does.
  (-> env
      (update :declared conj s)
      (update :names dissoc s)))

(defn bind [env s val]
  (-> env
      (assoc-in [:names s] (ns-ref val env))
      (update :declared disj s)))

(defn μ-declare [env μ]
  (trace! "declare μ param" (:params μ))
  (let [env' (μ-declare-1 env (:params μ))]
    (if (:name μ)
      (μ-declare-1 env' (:name μ))
      env')))

(defn μ-bind [env μ args]
  (trace! "bind μ" (:params μ) args)
  (let [env' (bind env (:params μ) args)]
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
                    :env    env
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
            :default (fn [_ [h t]]
                       (throw (RuntimeException. (str h " is not applicable. " t))))}}})


(defn walk
  ([state env form] (walk state env form false))
  ([state env form early-stop?]
   (let [t (ast-type ((get-in rules [state :dispatch] identity) form))]
     (trace! (name state) "rule" t form)
     ;; (trace! "env:" (sort-by :names (keys (:names env))))
     (let [r (:rules (get rules state))
           v ((get r t (get r :default)) env form)]
       (trace! (name state) "post" (ast-type v) v)
       (if (and ((get-in rules [state :recur-check] (constantly false)) form v)
                ;; HACK: Is there no better way to do this?
                (not early-stop?))
         (do
           (trace! "recurring on" form "->" v)
           (recur state env v false))
         v)))))

;;;;; Builtins

(defn createμ [env args]
  (let [[name params body] (if (= 3 (count args)) args `[nil ~@args])]
    (with-meta (ast/μ name params body)
      (meta body)))) ; metadata carries debug info around, which is handy

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
  (reduce (fn [acc [sym val]] (bind acc sym val)) (empty-ns)
          (merge
           (tag-primitives macros ast/->Macro)
           (tag-primitives fns ast/->Primitive))))

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
        (trace! "sending on" chn)
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
(def bootxprl (str srcpath "boot.xprl"))
(def testxprl (str srcpath "test.xprl"))

(def env (atom base-env))

(defn go!
  ([env f]
   (walk :reduce env (ast/immediate f)))
  ([env f ccs]
   (schedule [(fn [_] (connect (go! env f) ccs))])
   (run!)))

(defn ev [s]
  (go! @env (:form (r/read (r/string-reader s)))))

(defn iev [s]
  (ast/inspect (ev s)))

(defn loadfile [envatom fname]
  (let [conts {(xkeys :env)    (fn [[sym value]] (swap! envatom bind sym value))
               ;; FIXME: This should log a warning. It's not a fatal error
               (xkeys :return) #(throw (RuntimeException. "return to top level!"))
               (xkeys :error)  (fn [x]
                                 (println "Error: " x))}]
    (loop [reader (r/file-reader fname)]
      (let [reader (r/read reader)
            form   (:form reader)]
        (if (= :eof form)
          @envatom
          (do
            (go! @envatom form (with-return conts println))
            (recur reader)))))))

(defmacro inspect [n]
  `(-> @env (get-in [:names (ast/symbol ~(name n)) :value]) ast/inspect))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @env))))
