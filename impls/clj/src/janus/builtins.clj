(ns janus.builtins
  (:require [janus.ast :as ast]
            [janus.i4 :as i]
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
    (i/reduce name env
      (i/with-return c
        (fn [name']
          (assert (instance? janus.ast.Symbol name') "Only Symbols can be names.")
          (i/eval body env
            (i/with-return c
              (fn [body']
                (let [def (with-meta body'
                            (merge defmeta
                                   (select-keys [:file :line :col] (meta form))))]
                  ;; (i/event! ::def.top {:name name' :body body'})
                  (rt/emit c [[(ast/keyword "return") name']
                              [(ast/keyword "env") {name' def}]]))))))))))

(defn emit [mac kvs env c]
  (let [coll (rt/ordered-collector (count kvs)
               #(rt/emit c (partition 2 %)))]
    (rt/push!
     (into []
           (comp
            (map-indexed
             (fn [i x]
               [(if (even? i)
                  ;; eval keys
                  (fn [x] (i/eval x env (i/with-return c #(coll i %))))
                  ;; reduce values
                  (fn [x] (i/reduce x env (i/with-return c #(coll i %)))))
                x]))
            cat)
           kvs))))

(defn capture [_ [form] dyn ccs]
  (let [ccs' (into {rt/unbound (fn [{:keys [ch-name msg]}]
                                 (rt/emit ccs rt/return [ch-name msg]))}
                   (map (fn [[k v]]
                          [k (fn [v]
                               (rt/emit ccs rt/return [k v]))]))
                   (dissoc ccs rt/unbound))]
    (i/eval form dyn ccs')))

(defn select [_ [p t f] dyn ccs]
  (letfn [(next [p']
            ;; (i/event! ::select.p p')
            (case p'
              true (i/reduce t dyn ccs)
              false (i/reduce f dyn ccs)))]
    (i/reduce p dyn (i/with-return ccs next))))

(def macros
  {
   ;; REVIEW: I don't think `def` actually needs to be builtin at all. But I
   ;; need statefuls and eval on maps before I can remove it.
   "def" xprl-def

   ;; The grail of bootstrapping would be to implement μ in xprl, but I don't
   ;; have the tools to do that yet.
   "μ"   i/createμ

   ;; "withcc" withcc
   "emit"   emit

   ;; Select can be a function, but then it's basically impossible to use
   ;; effectively in μs because we want it (intuitively) to perform the "switch"
   ;; as soon as the predicate resolves, but a function must wait for all args
   ;; to resolve.
   ;;
   ;; I could implement select in xprl using the smalltalk method. But I don't
   ;; know if I want to go that way yet.
   "select" select

   ;; returns what would have been emitted as data so that we can inspect/modify
   ;; it. The bundle of continuations passed down to the form being evaluated is
   ;; its only connection to the outside world. If we cut that cable, it's fully
   ;; sandboxed, if we tap it, we know exactly what it wants to do and can
   ;; choose what to allow by selectively forwrding.
   ;;
   ;; Note, however, that if a computation (form being evaluated) creates
   ;; channels either by calling (which creates return channels) or by passing
   ;; functions (μs) to withcc, then those messages will still be sent. In other
   ;; words, the autonomy of the internal subcomputation is respected.
   ;;
   ;; Autonomous from below, subordinate from above.
   ;;
   ;; Will emit potentially many times on its return channel. Use `into` to
   ;; collect emissions if desired, but remember that `into` doesn't return
   ;; until the subcomputation is finished, so it could deadlock if the
   ;; subcomputation requires a response from outside to continue.
   ;;
   ;; I haven't decided yet whether to allow passing channels. I think that's a
   ;; bad idea, but I'm not 100% certain that we can make a useful language
   ;; without. I suppose the smart thing to do would be to start without and see
   ;; how far I can get.
   "capture" capture
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
