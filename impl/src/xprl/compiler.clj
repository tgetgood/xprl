 ;; REVIEW: I know this is throwaway code, but it's really bad, even for that.
;; I'm not going to be able to read this in a week, let alone debug it.
;;
;; But at least it's out of my head and on paper...
(ns xprl.compiler
  (:refer-clojure :exclude [resolve eval apply])
  (:require [clojure.pprint :refer [pprint]]
            [xprl.ast :as ast]
            [xprl.builtins :as builtins]
            [xprl.env :as env]
            [xprl.system :as sys :refer [net return return-first]]))

(def ret (ast/xkey :return))

(declare walk)

(defn emit! [ctx kvs]
  ;; (println kvs)
  (assert (every? ast/keyword? (map first kvs)) "Improper emission")
  (clojure.core/apply net ctx (map (fn [kv] {:call sys/send! :args kv}) kvs)))

(def overrides
  (reduce (fn [acc [k v]] (assoc acc (get builtins/base-env (ast/symbol k)) v))
          {}
          {"emit" (fn [ctx _ tail]
                    (emit! ctx tail))
           "net"  (fn [ctx state tail]
                    (net ctx tail))}))

(defn resolve [ctx [state form]]
  ;; (println (type form) form)
  (cond
    (ast/input? form)    (return ctx (:id form))
    (ast/ref? form)      (return ctx (:binding form))
    (ast/symbolic? form) (throw (RuntimeException. (str "unbound symbol: " form)))
    true                 (assert false "unreachable!!")))

;; TODO: Each invocation of a μ needs its own set of "storage locations" (svs).
(defn uniquify [μ]
  μ)

(defn apply-μ [ctx [state μ arg]]
  ;; (println "apply-μ" μ "to" arg)
  ;; (println (keys (:sv-index (:exec μ))))
  ;; (pprint (:exec μ))
  ;; (println (:ctx ctx))
  (let [μ      (uniquify μ)
        input  (:id μ)
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

(defn call-extern [ctx [state f tail]]
  (if (= (:fn f) builtins/noop)
    (if-let [v (get overrides f)]
      (v ctx state tail)
      (throw (RuntimeException. (str "unimplemented runtime call: " f))))
    (return ctx (ast/call state f tail))))

(defn apply [ctx [state head tail]]
  ;; (println "applying" head tail state)
  (cond
    (ast/μ? head)          (let [sync (ast/sv "apply-μ?")]
                             (net ctx
                               {:call walk
                                :ctx  {ret sync}
                                :args [state tail]}
                               {:call apply-μ
                                :args [state head sync]}))
    (ast/external? head)   (net ctx {:call call-extern
                                     :args [state head tail]})

    true (throw (RuntimeException. (str head " is not applicable!")))))

(defn eval [ctx [state form]]
  (cond
    (ast/map? form)
    (net ctx
      {:call walk
       :args [state (into {} (map #(into [] (map ast/immediate) %)) form)]})
    (ast/coll? form)     (net ctx
                           {:call walk
                            :args [state (into (empty form) (map ast/immediate) form)]})
    (ast/pair? form)     (net ctx
                           {:call walk
                            :args [state (ast/application (ast/immediate (:head form))
                                                          (:tail form))]})
    (ast/symbolic? form) (net ctx {:call resolve :args [state form]})
    true                 (return ctx form)))

(defn walk [ctx [state form]]
  ;; (println "walking" form "with" state "in" )
  ;; (pprint ctx)
  (cond
    (ast/immediate? form)   (let [sync (ast/sv "walk-immediate")]
                              (net ctx
                                {:call walk
                                 :ctx  {ret sync}
                                 :args [state (:form form)]}
                                {:call eval
                                 :args [state sync]}))
    (ast/application? form) (let [sync (ast/sv "walk-application-")]
                              (net ctx
                                {:call walk
                                 :ctx  {ret sync}
                                 :args [state (:head form)]}
                                {:call apply
                                 :args [state sync (:tail form)]}))
    (ast/symbolic? form)    (let [sym (ast/symbol form)]
                              ;; (println "capture?" sym (env/captured? state sym))
                              (return ctx (if (env/captured? state sym)
                                            (ast/input sym (env/capid state sym))
                                            form)))

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
