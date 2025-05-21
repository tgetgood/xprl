(ns janus.interpreter
  (:refer-clojure :exclude [name])
  (:require
   [janus.ast :as ast]
   [janus.env :as env :refer [body form head kvs name params tail xnth]]
   [janus.telemetry :refer [trace!]]))

(declare walk)

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
  (let [t1 (ast/type sexp)]
    (if-let [subtree (get rule-tree t1)]
      (let [subexp (step sexp)
            t2 (ast/type subexp)]
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
