(ns xprl.ns
  (:require [xprl.ast :as ast]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-ns {})

(defn ns-intern [ns sym val]
  (assert (not (contains? ns (ast/symbol sym))) (str sym " cannot be redefined."))
  (assoc ns (ast/symbol sym) val))

;; N.B.: This is used for tooling. Don't delete it.
(defn lookup [env sym]
  (assert (contains? env sym) sym)
  (get env sym))

(defn bind
  "Returns `form` with any symbols occuring in `ns` bound appropriately.
  Only works on forms returned from the reader. Will fail to properly traverse
  AST nodes created during interpretation."
  [form ns]
  (cond
    ;; REVIEW: I talk a lot about homoiconicity, but we can only read in a
    ;; fraction (less than half) of the ast node types from text. I've created
    ;; print literals for them for debugging, but we really don't have anything
    ;; like homoiconicity here.
    ;;
    ;; What gives? Does it go wrong? I'm not trying to create another lisp. I'm
    ;; okay with breaking conventions, even ones that seem nonnegotiable.
    ;;
    ;; μs give us all of the flexibility I would want from homoiconicity and
    ;; metaprogramming. And we can generate anything from the small readable
    ;; subset. So in practice this restriction doesn't seem to matter.
    ;;
    ;; But I still need to think on it some more.
    (ast/immediate? form) (update form :form bind ns)
    (ast/pair? form)      (-> form (update :head bind ns) (update :tail bind ns))
    (ast/coll? form)      (into (ast/empty form) (map #(bind % ns)) form)
    (ast/symbol? form)    (if (contains? ns form) (ast/ref form (get ns form)) form)
    true                  form))
