(ns janus.interpreter
  (:refer-clojure :exclude [name])
  (:require
   [janus.ast :as ast]
   [janus.env :as env :refer [body form head kvs name params tail xnth]]
   [janus.debug :refer [trace!]]))

(declare walk)

;;;;; Application

(defn apply-μ [μ args]
  (trace! "invoke" μ "with" args)
  (walk (env/μ-bind μ args)))

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

(defn apply-primitive [head tail]
  (let [args (walk tail)]
    (trace! "pcall" head args)
    ;; REVIEW: This assumes that all primitives take a list as args.
    ;; That seems innocuous, but what are the ramifications?
    (if (and (ast/list? args) (:check head) args)
      (apply (:fn head) args)
      (ast/application head args))))

(defn apply-error [h t]
  (throw (RuntimeException. (str h " is not applicable, but was called with " t))))

(defn apply-head [h t]
  (let [h'  (walk h)
        app (ast/application h' t)]
    (if (evaluated? h')
      (walk app)
      app)))

;;;;; Reduction

(defn reduce-μ [μ]
  ;; FIXME: Just look at it...
  (let [n (name μ)
        n (when n (if (ast/symbol? n) n (walk n)))
        p (params μ)
        p (if (ast/symbol? p) p (walk p))
        b (body μ)
        b (if (ast/symbol? p) (walk (env/μ-declare n p b)) (walk b))]
    (ast/μ n p b)))

(defn reduce-emit [x]
  ;; If we had a predicate that asked "is `kvs` fully realised?" then we
  ;; wouldn't need this at all. I'm just not sure how to write that predicate,
  ;; and this seems simple enough that I don't need to worry about it.
  (ast/emission (walk (kvs x))))

(defn list-xform [f xs]
  (ast/list (reduce (fn [acc i] (conj acc (f (xnth xs i)))) [] (range (count xs)))))

(defn reduce-list [xs]
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
  (let [inner (walk x)]            ; evaulate the inner expression
    (if (evaluated? inner)         ; if it worked
      (walk (ast/immediate inner)) ; evaluate the result
      (ast/immediate inner))))     ; otherwise restore the outer immediate

;;;;; Tree walker

(def rules
  {[:I :S] eval-symbol ; aka lookup in environment
   [:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-step
   [:I :A] eval-step

   :I identity    ; (I V) => V. values are fixed points of eval.

   :L reduce-list ; Most structures are values, with these exceptions.
   :μ reduce-μ    ; TODO: Drop `reduce`. It's too common a term to override.
   :E reduce-emit ; REVIEW: Do we actually need to reduce into Emissions?

   [:A :I] apply-head ; (A head tail) => (A (walk head) tail)
   [:A :A] apply-head ;   iff `head` is unevaluated.

   ;; An emission which includes a message to :return can trigger off the
   ;; application. But the connection logic isn't sophisticated enough for this
   ;; yet.
   ;; [:A :E] apply-emit

   [:A :F] apply-primitive ; Two kinds of operators are built in.
   [:A :μ] apply-μ         ; I think that's sufficient. I might be wrong.

   :A apply-error}) ; REVIEW: Should application be extensible?

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
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp (env/local-env (step sexp)))
    (let [v (smart-call f sexp)]
      (trace! "result:" rule "\n" sexp "\n->\n" v)
      v)))

;;;;; Builtins

(defn partial? [args]
  (case (count args)
    2 (ast/symbol? (first args))
    3 (and (ast/symbol? (first args)) (ast/symbol? (second args)))))

(defn createμ [args]
  (let [[name params body] (case (count args)
                             3 args
                             2 `[nil ~@args])]
    (walk (with-meta (ast/μ name params body) (meta args)))))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (walk
   (with-meta
     (ast/emission
      (ast/list (map (fn [[k v]] [(ast/immediate k) v]) (partition 2 kvs))))
     (meta kvs))))

(defn check-select [args]
  (boolean? (first args)))

(defn select [[p t f]]
  ;; `t` & `f` have already been walked, so we've nothing to do but pick one.
  (if p t f))
