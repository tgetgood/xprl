(ns xprl.executor
  (:require [xprl.ast :as ast]))

(defn deliver! [dest msg]
  (assert (not (nil? dest)))
  (cond
    (ast/wire? dest) (run! (fn [[_ conn]] (deliver! conn msg)) @(:connections dest))
    (fn? dest) (dest msg)
    true (throw (RuntimeException.
                 (str "Cannot deliver message to a " (type dest) ":\n" dest)))))

(defn msgs [form]
  (cond
    (ast/emission? form) (into [] (:msgs form))
    true                 []))


(defn execute! [[dest msg]]
  (if-let [next (deliver! (:wire (meta dest)) msg)]
    (msgs next)
    []))

(defn start! [form]
  (loop [work (msgs form)]
    (when (seq work)
      (recur (into (pop work) (execute! (peek work)))))))

;; TODO: take the cable in here, thread the form and create the initial queue,
;; then run to empty.
;;
;; I think the system logic from the compiler branch (which I deleted here) is
;; actually the right way to go.
