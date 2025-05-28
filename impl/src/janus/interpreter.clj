(ns janus.interpreter
  (:refer-clojure :exclude [name])
  (:require
   [janus.ast :as ast]
   [janus.env :as env :refer [body form head kvs name params tail]]
   [janus.debug :as debug :refer [trace!]]))

(declare walk)

(defn evaluated? [x]
  (cond
    (instance? janus.ast.Immediate x)   false
    (instance? janus.ast.Application x) false
    true                                true))

;;;;; Application

(defn apply-μ [app]
  (let [μ    (head app)
        args (tail app)]
    (trace! "invoke" μ "with" args)
    (walk (env/μ-bind μ args))))

(defn apply-primitive [app]
  (let [h    (head app)
        args (walk (tail app))]
    (trace! "pcall" h args)
    ;; REVIEW: This assumes that all primitives take a list as args.
    ;; That seems innocuous, but what are the ramifications?
    (if (and (ast/list? args) ((:check h) (env/elements args)))
      ((:fn h) (env/elements args))
      (ast/application h args))))

(defn apply-error [app]
  (throw (RuntimeException.
          (str (head app) " is not applicable, but was called with " (tail app)
               "\n" (debug/provenance app)))))

(defn apply-head [app]
  (let [app (ast/application (walk (head app)) (tail app))]
    (if (evaluated? (head app))
      (walk app)
      app)))

;;;;; Eval

(defn eval-symbol [im]
  (let [s   (form im)
        env (env/get-env s)]
    (if-let [ref (env/lookup env s)]
      (do
        (trace! "resolve" s ":" ref)
        (walk ref))
      (do
        (trace! (if (contains? (env/decls env) s) "declared" "undeclared") s)
        im))))

(defn eval-list [im]
  (walk (env/map-list ast/immediate (form im))))

(defn eval-pair [im]
  (let [p (form im)]
    (walk (ast/application (ast/immediate (head p)) (tail p)))))

(defn eval-step [im]
  (let [im (ast/immediate (walk (form im)))]
    (if (evaluated? (form im))
      (walk im)
      im)))

;;;;; Reduction

(defn reduce-μ [μ]
  ;; FIXME: Just look at it...
  (let [n (name μ)
        p (params μ)
        b (body μ)
        n' (when n (if (ast/symbol? n) n (walk n)))
        p' (if (ast/symbol? p) p (walk p))
        b' (if (ast/symbol? p) (walk (env/μ-declare n p b)) (walk b))]
    (ast/μ n' p' b')))

(defn reduce-emit [e]
  ;; If we had a predicate that asked "is `kvs` fully realised?" then we
  ;; wouldn't need this at all. I'm just not sure how to write that predicate,
  ;; and this seems simple enough that I don't need to worry about it.
  (ast/emission (walk (kvs e))))

(defn reduce-list [l]
  (env/map-list walk l))

;;;;; Tree walker

(def rules
  {[:I :S] eval-symbol ; aka lookup in environment
   [:I :P] eval-pair   ; (I (P x y)) => (A (I x) y)
   [:I :L] eval-list   ; (I (L x y ...)) => (L (I x) (I y) ...)
   [:I :I] eval-step
   [:I :A] eval-step

   :I form        ; (I V) => V. values are fixed points of eval.

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

(def rule-tree
  (reduce (fn [acc [k v]]
            (assoc-in acc (if (vector? k) (conj k :fn) [k :fn]) v))
          {} rules))

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

(defn walk [sexp]
  ;; (trace! "walk" sexp "\n" (ast/ctx sexp) "\n" (ast/ctx (step sexp)))
  (let [[rule f] (rule-match sexp)]
    (trace! "rule match:" rule sexp
            "\n  symbols:" (ast/symbols (step sexp))
            (env/local-env (step sexp)))
    (let [v (f sexp)]
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
    (walk (ast/μ name params body))))

(defn emit [kvs]
  (assert (even? (count kvs)))
  (walk
   (ast/emission
    (ast/list (map (fn [[k v]] (ast/list [(ast/immediate k) v]))
                   (partition 2 kvs))))))

(defn check-select [args]
  (boolean? (first args)))

(defn select [[p t f]]
  ;; `t` & `f` have already been walked, so we've nothing to do but pick one.
  (if p t f))
