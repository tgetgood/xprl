(ns xprl.continuation
  (:refer-clojure :exclude [merge])
  (:require [xprl.ast :as ast]))

(def ret (ast/xkey :return))

(defn return {:style/indent 1} [env x]
  (let [retfn (get env ret)]
    (assert (fn? retfn) (str "Cannot return " x ". No destination."))
    ;; REVIEW: I skip the executor for local returns because it's simple and
    ;; I'm afraid of everything grinding to a halt if I don't. This could be
    ;; premature optimisation and I should look here first if there are weird
    ;; bugs.
    (retfn x)
    ;; don't trust all return continuations to return nil here!
    nil))

(defn with-return [env retfn]
  (assoc env ret retfn))

(defn merge [env extras]
  (clojure.core/merge env extras))

(defn ret-> {:style/indent [1]} [env inner outer]
  (inner (with-return env outer)))

(defn error! [env msg]
  ((get env (ast/xkey :error)) msg))
