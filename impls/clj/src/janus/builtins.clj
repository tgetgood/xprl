(ns janus.builtins
  (:require [janus.ast :as ast]
            [janus.interpreter :as i]
            [janus.runtime :as rt]
            [janus.util :refer [form-error!]]))

(defn validate-def [form]
  (when-not (<= 2 (count form) 3)
    (form-error! form "invalid args passed to def."))

  )

(defn xprl-def [form env c]
  (let [name (first form)
        doc  (if (and (= 3 (count form)) (string? (second form))) (second form) "")
        body (last form)])
  (rt/emit c (ast/keyword "env") (assoc (:env (meta (first form)))
                                        (first form) (last form))
             (ast/keyword "return") (last form)))

(def macros
  {"def" xprl-def
   "μ"   i/createμ
   ;; "withcc" withcc
   ;; "emit"   emit
   })

(def fns
  {"+*"     +
   "**"     *
   "-*"     -
   ;; REVIEW: Is there a better way to do data (non-branching) selection in
   ;; clojure?
   "select" (fn [p t f]
              (condp identical? p
                ast/t t
                ast/f f))})

(def base-env
  (merge
   (into {} (map (fn [[k v]] [(ast/symbol k) (ast/->PrimitiveMacro v)])) macros)
   (into {} (map (fn [[k v]] [(ast/symbol k) (ast/->PrimitiveFunction v)])) fns)))
