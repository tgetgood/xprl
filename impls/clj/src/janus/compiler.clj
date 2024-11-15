(ns janus.compiler
  (:refer-clojure :exclude [var symbol keyword]))

(def ast-transforms
  {
   ;; Standard (eval f) => (apply (eval (first f)) ...)
   (immediate (pair (var :x) (var :y))) (application (immediate (var :x)) (var :y))

   ;; primitive macros can execute directly
   (application (pmacro (var :f)) (var :x)) ((var :f) (var :x))

   ;; Applying args to a μ extends the env of the body
   (application (μ (var :s) (var :body)) (var :y))
   ;; TODO: it must be possible to unify (var :s) and (var :y) to proceed
   (context {(var :s) (var :y)} (var :body))

   ;; Syntactic renaming.
   (application (μ (var :x) (var :body)) (immediate (var :y)))
   (rename {(var :x) (var :y)} (var :body))

   ;; renaming noop (special case).
   (application (μ (var :x) (var :body)) (immediate (var :x))) (var :body)

   })
