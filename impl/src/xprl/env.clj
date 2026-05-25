(ns xprl.env
  (:refer-clojure :exclude [bound? binding])
  (:require [clojure.set :as set]
            [xprl.ast :as ast]))

(defn with-env [env x]
  (if (ast/env? x)
    (assoc x :env env)
    x))

(defn merge-envs [inner outer]
  (merge-with merge outer inner))

(defn merge-local [env x]
  (if (ast/env? x)
    (merge-envs env (:env x))
    env))

(defn bind [env id val]
  (-> env
      (assoc-in [:bindings id] [env val])))

(defn bound? [env input]
  (contains? (:bindings env) (:id input)))

(defn binding [env form]
  (get-in env [:bindings (:id form)]))

(defn unbind
  "Removes parameter bindings from nested invocations of the same function."
  [env μ]
  (update env :bindings dissoc (:id μ)))

(defn capture [s sym id]
  (update s :captured assoc sym id))

(defn captured? [s sym]
  (contains? (:captured s) sym))

(defn capid [s sym]
  (get-in s [:captured sym]))

(defn uncapture [s sym]
  (update s :captured dissoc sym))

(defmacro walk-cond
  "Separate tree traversal from the important logic."
  [form walk & cases]
  {:syle/indent [2]}
  `(cond
     (ast/immediate? ~form)   (update ~form :form ~walk)
     (ast/application? ~form) (-> ~form (update :head ~walk) (update :tail ~walk))
     (ast/pair? ~form)        (-> ~form (update :head ~walk) (update :tail ~walk))
     (ast/coll? ~form)        (into (ast/empty ~form) (map ~walk) ~form)
     (ast/emission? ~form)    (update ~form :msgs ~walk)
     ~@cases
     true                     ~form ))

(defn walk-capture [sym input form]
  (let [walk (partial walk-capture sym input)]
    (walk-cond form walk
      ;; FIXME: If we're creating nested μs from the outside in, then we'll need
      ;; to clobber inputs in narrower contexts. That's correct, but there might
      ;; be cases where it leads to problems.
      (ast/symbolic? form) (if (= (ast/symbol form) sym) input form)
      (ast/μ? form)        (if (= sym (:param form))
                             form
                             (update form :body walk)))))

(defn walk-bind [bindings form]
  (let [walk (partial walk-bind bindings)]
    (walk-cond form walk
      (ast/input? form) (if (contains? bindings (:id form))
                          (with-env (bind (:env form) (:id form)
                                          (get bindings (:id form)))
                            form)
                          ;; REVIEW: Walk binding?
                          form)
      (ast/μ? form)     (update form :body walk))))
