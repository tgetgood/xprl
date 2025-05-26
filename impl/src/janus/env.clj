(ns janus.env
  (:refer-clojure :exclude [name])
  (:require
   [clojure.set :as set]
   [janus.ast :as ast]
   [janus.debug :refer [trace!]]))

(defn empty-ns []
  ;; REVIEW: One sanity check on a namespace is that the set of declared (but
  ;; not instantiated) names must be empty by the time we finish reading it in.
  {:names {} :declared #{}})

(defn syms [ns]
  (sort-by :names (keys (:names ns))))

(defn lookup [env sym]
  (get-in env [:names sym]))

(defn decls [env]
  (:declared env))

(defn get-env [x]
  (::env (ast/ctx x)))

(defn with-env [x env]
  (ast/with-ctx x
    (assoc (ast/ctx x) ::env env)))

(defn ns-bind
  "Like `bind`, but skips the declaration check since forms interned into a
  namespace don't need to be declared first (but they can be, so we still need
  to clear declarations)."
  [ns s val]
  (-> ns
      (assoc-in [:names s] val)
      (update :declared disj s)))

(defn bind [env s val]
  (assert (contains? (decls env) s) (str "binding undeclared name:" s "to" val))
  (-> env
      (assoc-in [:names s] val)
      (update :declared disj s)))

(defn μ-declare-1 [env s]
  (-> env
      (update :declared (fnil conj #{}) s)
      (update :names dissoc s)))

(def ^:dynamic env (atom (empty-ns)))

(defn local-env [form]
  (let [env  (get-env form)
        syms (ast/symbols form)]
    (str "\n  defined: "
         (into {} (filter #(contains? syms (key %)) (:names env)))
         " declared: " (decls env))))

;;;;; env preserving ast traversal

(defn bind-decls [inner outer]
  (when (seq inner)
    ;; REVIEW: this is ugly and so likely wrong.
    ;;
    ;; Defined symbols can't "bleed down" into a contained subcontext. But
    ;; declarations can. Why?
    ;;
    ;; The only impetus right now is indirectly defined μs where the body is
    ;; initially walked in a context where the μ of which it will eventually be
    ;; part does not yet exist. In this case, when we later create the outer μ
    ;; and walk ~it~, the body needs to be walked in a new context that didn't
    ;; exist when it was walked the first time. The only difference between
    ;; these contexts *should* be the declaration of the parameter (and any
    ;; others if we have a stack of such μs above us),
    ;;
    ;; BUT: can we prove that a μ so constructed will always be reduced ~as a μ~
    ;; before arguments are applied to it? If not, then the applied args will
    ;; never get set because their parameters haven't yet been declared.
    (let [extra-decls (set/difference (decls outer)
                                      (decls inner)
                                      (into #{} (keys (:names inner))))]
      (update (reduce (fn [e sym]
                        (if-let [val (lookup outer sym)]
                          (bind e sym val)
                          e))
                      inner
                      (decls inner))
              :declared set/union extra-decls))))

(defn merge-env [x y]
  (let [outer (or (get-env x) nil)
        inner (or (get-env y) outer)]
    (bind-decls inner outer)))

(defn nearest-env [x k]
  (let [v (get x k)
        e (merge-env x v)]
    (with-env v e)))

(defn params [x] (nearest-env x :params))
(defn name   [x] (nearest-env x :name))
(defn body   [x] (nearest-env x :body))
(defn head   [x] (nearest-env x :head))
(defn tail   [x] (nearest-env x :tail))
(defn form   [x] (nearest-env x :form))
(defn kvs    [x] (nearest-env x :kvs))
(defn els    [x] (nearest-env x :elements))
(defn xnth [x n] (nearest-env (els x) n))

;;;;; μ specfics

(defn μ-declare [name params body]
  (let [env (μ-declare-1 (get-env body) params)
        env (if name (μ-declare-1 env name) env)]
    (trace! "declaring params:" params name (local-env body))
    (with-env body env)))

(defn μ-bind [μ args]
  (let [body (body μ)]
    (if-not (ast/contextual? body)
      (do
        (trace! "Primitive μ body detected, not binding params.")
        body)
      (let [env (bind (get-env body) (params μ) args)
            env (if (name μ) (bind env (name μ) μ) env)]
        (trace! "binding params:" (params μ) "to" args (local-env args))
        (with-env body env)))))
