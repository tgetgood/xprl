(ns xprl.compiler
  (:refer-clojure :exclude [resolve eval apply compile])
  (:require [clojure.pprint :refer [pprint]]
            [xprl.ast :as ast]
            [xprl.env :as env]
            [xprl.system :as sys :refer [net return return-first ret]]))

(declare walk)

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
                             {:call apply-μ
                              :args [state head sync]}))
    (ast/external? head) (net ctx {:call (fn [ctx [state head tail]]
                                           (ast/call :compiled ctx state head tail))
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

(defn μ-walk* [state form]
  ;; (println "μ-walk" form)
  (cond
    (ast/application? form) (let [tail (μ-walk state (:tail form))
                                  sync (ast/sv "μ-walk-application?")
                                  head (μ-walk state (:head form))
                                  ht   [{:call net/apply
                                         :ctx  {ret sync}
                                         :args [state (:return head) (:return tail)]}]]
                              {:return sync
                               :tasks  (into [] (concat ht
                                                        (:tasks head)
                                                        (:tasks tail)))})
    (vector? form)          (let [vform (mapv (partial μ-walk state) form)
                                  sync  (ast/sv "μ-walk-vector?")]
                              {:return sync
                               :tasks  (into [{:call return
                                               :ctx  {ret sync}
                                               :args (into [] (map :return) vform)}]
                                             (mapcat :tasks)
                                             vform)})
    (ast/input? form)       {:return (:id form)}
    (ast/immediate? form)   (let [f    (μ-walk state (:form form))
                                  sync (ast/sv "μ-walk-immediate?")]
                              {:return sync
                               ;; REVIEW: This calls `eval` at runtime!
                               ;; That's fine if we're sure that the arg is
                               ;; symbolic. But if it has to call `walk`, then
                               ;; we need the whole interpreter!
                               :tasks  (conj (:tasks f)
                                             {:call net/eval
                                              :ctx  {ret sync}
                                              :args [state (:return f)]})})
    (ast/μ? form)           (let [svs (:waiting (:exec form))]
                              (if (empty? svs)
                                {:return form}
                                (let [sync   (ast/sv "μ-walk-μ?")
                                      sv-vec (into [] svs)] ; fix the order!
                                  {:return sync
                                   :tasks  [{:call (fn [ctx vs]
                                                     (return ctx (closeμ form sv-vec vs)))
                                             :ctx {ret sync}
                                             :waiting svs
                                             :args sv-vec}]})))
    true                    {:return form}))

(defn compile [env {:keys [form] :as ctx}]
  (cond
    (ast/immediate? form) (eval env (compile env (update ctx :form :form)))
    (ast/input? form)     (assoc ctx :return (:id form) :form ::EOE)
    (ast/symbolic? form)  ()))
