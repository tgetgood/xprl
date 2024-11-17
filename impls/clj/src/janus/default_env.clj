(ns janus.default-env
  (:require [janus.ast :as ast]
            [janus.interpreter :as i]
            [janus.runtime :as rt]))

(defn xprl-def [form env c]
  (rt/emit c ;(ast/keyword "return") (last form)
           (ast/keyword "env") (assoc (:env (meta (first form)))
                                      (first form) (last form))))

(def macros
  {"def" xprl-def
   "μ"   i/createμ
   ;; "withcc" withcc
   ;; "emit"   emit
   ;; "select" select
   })

(def fns
  {"+*"     +
   "**"     *
   "-*"     -
   ;; REVIEW: Is there a better way to do data (non-branching) selection in clojure?
   "select" (fn [p t f]
              (case p
                ast/t t
                ast/f f
                (throw (RuntimeException. "selecting on non boolean."))))})

(def env
  (merge
   (into {} (map (fn [[k v]] [(ast/symbol k) (ast/->PrimitiveMacro v)])) macros)
   (into {} (map (fn [[k v]] [(ast/symbol k) (ast/->PrimitiveFunction v)])) fns)))
