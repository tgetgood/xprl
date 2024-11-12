(ns janus.default-env
  (:require [janus.ast :as ast]))

(defn pfun [f]
  (ast/->PrimitiveMacro f))

(defn xprl-def [])

(defn createμ [])

(def macros
  {"def"    xprl-def
   "μ"      createμ
   ;; "withcc" withcc
   ;; "emit"   emit
   ;; "select" select
   })
