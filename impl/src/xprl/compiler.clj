(ns xprl.compiler
  (:refer-clojure :exclude [resolve eval apply compile])
  (:require [clojure.pprint :refer [pprint]]
            [xprl.ast :as ast]
            [xprl.env :as env]
            [xprl.system :as sys :refer [net return return-first]]))

(def ret (ast/xkey :return))

(declare walk)

;; TODO: Even with the compiler cache, we still need a two pass compiler, at the
;; very least, to correctly handle recursion. Naive compilation, in particular,
;; will regress infinitely.
(def ^:private compiler-cache (atom {}))

(defn compile [state μ]
  )

(defn resolve [ctx [state form]]
  (cond
    (ast/input? form)    (return ctx (:id form))
    (ast/ref? form)      (return ctx (:binding form))
    (ast/symbolic? form) (throw (RuntimeException. (str "unbound symbol: " form)))
    true                 (assert false "unreachable!!")))

(defn apply-μ [ctx [state μ arg]]
  (let [input  (:id μ)
        replay (select-keys (:sv-cache ctx) (keys (:sv-index (:exec μ))))]
    (clojure.core/apply
     net (sys/merge-ctx ctx (:exec μ))
      {:call return-first
       :args [(:return μ)]}
      {:call return-first
       :ctx  {ret (:id μ)}
       :args [arg]}
      (map (fn [[sv val]]
             {:call return-first
              :ctx {ret sv}
              :args [val]})
           replay))))

(defn apply [ctx [state head tail]]
  (cond
    (ast/μ? head)        (let [sync (ast/sv "apply-μ?")]
                           (net ctx
                             {:call walk
                              :ctx  {ret sync}
                              :args [state tail]}
                             {:call apply-μ
                              :args [state head sync]}))
    (ast/external? head) (net ctx {:call (fn [ctx [state head tail]]
                                           (ast/call :compiled ctx head tail))
                                   :args [state head tail]})

    true (throw (RuntimeException. (str head " is not applicable!")))))

(defn walk [ctx [state form]]
  (cond
    (ast/immediate? form)   (let [sync (ast/sv "walk-immediate")]
                              (net ctx
                                {:call walk
                                 :ctx  {ret sync}
                                 :args [state (:form form)]}
                                {:call (fn [ctx [state form]]
                                         (if (ast/symbolic? form)
                                           (net ctx {:call resolve :args [state form]})
                                           (throw (RuntimeException.
                                                   "Eval at runtime!"))))
                                 :args [state sync]}))
    (ast/application? form) (let [sync (ast/sv "walk-application-")]
                              (net ctx
                                {:call walk
                                 :ctx  {ret sync}
                                 :args [state (:head form)]}
                                {:call apply
                                 :args [state sync (:tail form)]}))

    (ast/recurser? form) (return ctx (compile state (:μ? form)))
    (ast/μ? form)        (return ctx (compile state form))

    (ast/coll? form) (let [syncs (take (count form) (repeatedly #(ast/sv "walk-coll")))]
                       (clojure.core/apply
                        net ctx
                        {:call (fn [ctx coll]
                                 (return ctx (into (ast/empty form) coll)))
                         :args syncs}
                        (map (fn [f sync]
                               {:ctx  {ret sync}
                                :call walk
                                :args [state f]})
                             form syncs)))
    true             (return ctx form)))


(defn entry [form ccmap]
  (net (sys/empty-ctx ccmap)
    {:call walk
     :args [{} form]}))
