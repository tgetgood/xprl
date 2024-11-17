(ns janus.default-env
  (:require [janus.ast :as ast]
            [janus.interpreter :as i]))

(defn xprl-def [])

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
