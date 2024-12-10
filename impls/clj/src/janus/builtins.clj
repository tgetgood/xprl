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

(defn xprl-def [form args env c]
  (let [[name body defmeta] (validate-def c args)]
    (letfn [(next [name']
              (cond
                (instance? janus.ast.Symbol name')
                (letfn [(next [body']
                          (let [def body']
                            (i/event! ::def.top {:name name' :body body'})
                            (rt/emit c
                                     (ast/keyword "env")   {name' def}
                                     (ast/keyword "return") name')))]
                  (i/event! ::def.evalbody {:body body :name name'})
                  (i/eval body env (i/with-return c next)))

                (i/reduced? name')
                (fatal-error! c name' "Can only bind Symbols in env.")

                :else (do
                        (i/event! ::def.delay {:form form :args args :env env
                                               :name name'})
                        (i/succeed c (ast/application form args env)))))]
      (i/reduce name env (i/with-return c next)))))

(defn emit [mac kvs env c]
  (letfn [(next [v]
            (if (i/reduced? v)
              (apply rt/emit c v)
              (i/succeed c (with-meta (ast/application
                                       mac (with-meta v
                                             (assoc (meta v)
                                                    :reduced? true)))
                             (meta mac)))))]
    (let [coll (rt/collector (i/with-return c next) (count kvs))
          tasks
          (into
           []
           (comp
            (map-indexed
             (fn [i x]
               [(if (even? i)
                  ;; eval keys
                  (fn [x] (i/eval x env (i/with-return
                                          c #(rt/receive coll i %))))
                  ;; reduce values
                  (fn [x] (i/reduce x env (i/with-return
                                            c #(rt/receive coll i %)))))
                x]))
            cat)
           kvs)]
      (apply rt/emit c tasks))))

(defn clear [ctx]
  (dissoc ctx :form :head :tail))

#_(defn select [[p t f] dyn ccs]
  (let [marker (gensym)]
    (-> ctx
        clear
        (assoc :form p)
        (assoc-in [:continuations rt/return]
                  (fn [{p' :form}]
                    (assert (boolean? p)
                            "Select only takes a literal boolean as first arg.")
                    (-> ctx
                        clear
                        (assoc :return-from ctx)
                        (assoc :form (if p' t f))
                        i/reduce)))
        i/reduce)))

(def macros
  {"def" xprl-def
   "μ"   i/createμ
   ;; "withcc" withcc
   "emit"   emit

   ;; Select can be a function, but then it's basically impossible to use
   ;; effectively in μs because we want it (intuitively) to perform the "switch"
   ;; as soon as the predicate resolves, but a function must wait for all args
   ;; to resolve.
   ;; "select" select

   ;; returns what would have been emitted as data so that we can inspect/modify
   ;; it.
   ;; "capture" capture

   })

(defn nth* [c i]
  (nth c (dec i)))

(def fns
  {"+*" +
   "**" *
   "-*" -
   "/*" /
   ">*" >
   "<*" <
   "=*" =

   "nth*" nth* ; Base 1 indexing
   })

(defn tagged [f]
  (fn [[k v]]
    (let [k' (ast/symbol k)]
      [k' (f (with-meta v {:name k'}))])))

(def base-env
  (merge
   (into {} (map (tagged ast/->PrimitiveMacro)) macros)
   (into {} (map (tagged ast/->PrimitiveFunction)) fns)))
