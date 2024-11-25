(ns janus.builtins
  (:require [janus.ast :as ast]
            [janus.interpreter :as i]
            [janus.runtime :as rt]
            [janus.util :refer [fatal-error!]]
            [taoensso.telemere :as t]))

(defn validate-def [c form]
  (when-not (<= 2 (count form) 3)
    (fatal-error! c form "invalid args to def"))
  (when (and (= 3 (count form)) (not (string? (second form))))
    (fatal-error! c form "Invalid docstring to def"))

  (let [name (first form)
        doc  (if (= 3 (count form)) (second form) "")
        body (last form)]
    [name body (assoc (meta form) :doc doc :source body)]))

(defn xprl-def [form c]
  (let [[name body defmeta] (validate-def c form)]
    (letfn [(next [cform]
              (let [def  (with-meta cform (merge (meta cform) defmeta))
                    lex' (assoc (:lex (meta form)) name def)]
                (t/event! :def/top {:level :trace :data [name cform]})
                (rt/emit c
                  (ast/keyword "env")    lex'
                  (ast/keyword "return") name)))]
      (t/event! :def/evalbody {:level :trace :data body})
      (i/eval body (rt/withcc c rt/return next)))))


(def macros
  {"def" xprl-def
   "μ"   i/createμ
   ;; "withcc" withcc
   ;; "emit"   emit
   })


(def plus (fn [& args] (reduce + args)))

(def fns
  {"+*" +
   "**" *
   "-*" -
   "/*" /
   ">*" >
   "<*" <
   "=*" =

   "nth*" (fn [c i] (nth c (dec i))) ; Base 1 indexing

   ;; REVIEW: Is there a better way to do data (non-branching) selection in
   ;; clojure?
   ;; REVIEW: The smalltalk style if can be implemented in xprl without any
   ;; builtins. That's very elegant, but does it have other advantages?
   ;; The downside is that I would have to implement all of the boolean builtins
   ;; myself instead of just calling them.
   "select" (fn [p t f]
              (condp identical? p
                true t
                false f))})

;; TODO: Set metadata on primitives
(def base-env
  (merge
   (into {} (map (fn [[k v]] [(ast/symbol k) (ast/->PrimitiveMacro v)])) macros)
   (into {} (map (fn [[k v]] [(ast/symbol k) (ast/->PrimitiveFunction v)])) fns)))
