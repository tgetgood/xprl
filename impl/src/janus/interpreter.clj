(ns janus.interpreter
  (:refer-clojure :exclude [name])
  (:require
   [janus.ast :as ast]
   [janus.env :as env :refer [body form head kvs name params tail xnth]]
   [janus.debug :refer [trace!]]))

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
        (env/with-env (assoc μ' :body (walk body)) (env/get-env body))))))

(defn emit-reduce [x]
  (ast/emission (walk (kvs x))))

(defn list-xform [f xs]
  (ast/list (reduce (fn [acc i] (conj acc (f (xnth xs i)))) [] (range (count xs)))))

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

(defn rule-match [sexp]
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
  (let [[rule f] (rule-match sexp)
        _        (trace! "rule match:" rule sexp (env/locals (env/merge-env sexp (step sexp))))
        v        (smart-call f sexp)]
    (trace! "result:" rule "\n" sexp "\n->\n" v)
    v))

;;;;; Builtins

;; REVIEW: `μ` needs to be a macro if we want to insist that an undefined symbol
;; cannot be present in a valid sexp.
;;
;; If we allow unbound symbols and just defer evaluation of them, then there's
;; no reason `createμ` is a predicate primitive which delays until its first two
;; arguments are reduced to symbols.
;;
;; We wouldn't need the partial μ logic above in this case.
;;
;; The soul of the language is the idea that a symbol cannot change its meaning
;; during a computation. That doesn't mean a symbol can't have no known meaning,
;; does it? It just means that once we establish the meaning of a symbol, that's
;; it.
;;
;; of course I'm allowing shadowing, so even that above statement isn't really
;; true unless you accept the conceit that different names can have the same
;; name. That isn't controversial in mathematics where most functions use x,y,z
;; as variables and we rename them willy nilly when it behooves us.
;;
;; Static single assignment is a great idea in llvm, but it only holds within a
;; given block for a reason: global uniqueness would make modularity impossible.
;;
;; This language I'm building is reflective down to its roots, so if you look at
;; code built by deeply nested μs, everything is an `x`, but those `x`s are all
;; different. We track the context in which that `x` was used and so we
;; effectively have subscripts on all of the `x`s that make them unique.
;;
;; I'm rambling, but sometimes its useful to overthink things out loud.
(defn createμ [args]
  (let [[name params body] (case (count args)
                             3 args
                             2 `[nil ~@args])]
    (walk (with-meta (ast/μ name params body) (meta args)))))

;; REVIEW: does `emit`` need to be a macro? We want to modify the keys (channel
;; names) by evaluating them, but so long as AST constructors and `walk` itself
;; are functions, `emit` can just be a normal primitive function, can it not?
;;
;; We wouldn't need special rules to reduce and apply Emissions in this case.
;; But we would still need special logic ~somewhere~ to connect to a buried
;; emission and resume the computation if it emits to :return.
;;
;; I think I should get ν incorporated into the language before worrying about
;; that.
(defn emit [kvs]
  (assert (even? (count kvs)))
  (walk
   (with-meta
     (ast/emission
      (ast/list
       (into [] (map (fn [[k v]] [(ast/immediate k) v])) (partition 2 kvs))))
     (meta kvs))))

;; REVIEW: Does `select` really need to be a macro? We're not preventing the
;; walking of subforms; both `t` & `f` get walked whatever happens.
;;
;; One issue is pruning. We can and should apply the select as soon as `p` is
;; reduced, without waiting for both `t` & `f`. A Primitive would wait for all
;; args to be evaluated before applying.
;;
;; Instead of two kinds of primitives, maybe we want instead to think about
;; having just one kind which parametrises on when it can run. A predicate for
;; deciding whether to run the primitive or delay would suffice.
(defn select {:name "select"} [args]
  (let [[p t f] (map #(walk (xnth args %)) (range 3))]
    (cond
      (boolean? p)         (if p t f)
      (not (evaluated? p)) (ast/application (ast/macro #'select) [p t f]))))
