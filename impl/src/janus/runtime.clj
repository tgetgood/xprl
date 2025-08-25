(ns janus.runtime
  (:require
   [janus.ast :as ast]
   [janus.debug :refer [trace!]]
   [janus.env :as env]
   [janus.interpreter :as i]))

(defn with-return [ccs cont]
  (assoc ccs (ast/xkeys :return) cont))

(defn send! [ccs chn msg]
  (let [err     (fn [_] (throw (RuntimeException. (str "No such channel: " chn))))
        unbound #((get ccs (ast/xkeys :unbound) err) [chn %])]
    ((get ccs chn unbound) msg)))

(defn perform-emit! [x ccs]
  (loop [kvs (i/walk (:kvs x))]
    (when (seq kvs)
      (let [[chn msg] (first kvs)]
        (trace! "sending on" chn ":" msg)
        (send! ccs chn msg))
      (recur (rest kvs)))))

(defn send-return! [v ccs]
  (send! ccs (ast/xkeys :return) v))

(declare connect)

(def connection-rules
  {:E perform-emit!})

(defn connection [x]
  (get connection-rules (ast/type x) send-return!))

(defn connect [form ccs]
  ((connection form) form ccs))
