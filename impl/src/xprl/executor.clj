(ns xprl.executor
  (:require [xprl.ast :as ast]
            [xprl.interpreter :as i]))

(defn deliver! [cable k env msg]
  (let [dest (get cable k)]
    (assert (not (nil? dest)))
    (cond
      (ast/wire? dest) (run! (fn [[_ conn]] (deliver! conn msg)) @(:connections dest))
      (fn? dest)       (dest (i/walk env msg))
      (ast/μ? dest)    (i/apply env dest msg)
      true             (throw (RuntimeException.
                               (str "Cannot deliver message to a " (type dest) ":\n" dest))))))

(defn msgs [form]
  (cond
    (ast/emission? form) (into [] (map (fn [x] (into [(:env form)] x))) (:msgs form))
    true                 []))


(defn execute! [[dest msg]]
  (if-let [next (deliver! (:wire (meta dest)) msg)]
    (msgs next)
    []))

(defn start! [cable form]
  (if (ast/emission? form)
    (run! (fn [[env k msg]] (deliver! cable k env msg)) (msgs form))
    form)
  #_(loop [work (msgs form)]
    (when (seq work)
      (recur (into (pop work) (execute! (peek work)))))))

;; TODO: take the cable in here, thread the form and create the initial queue,
;; then run to empty.
;;
;; I think the system logic from the compiler branch (which I deleted here) is
;; actually the right way to go.
