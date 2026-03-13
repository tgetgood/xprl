(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(deftracefn apply [s e head tail]
  (cond
    (ast/μ? head)          (ast/call head (walk s e tail))
    (ast/external? head)   ((:fn head) s e head tail)
    (ast/incomplete? head) (ast/application e head (walk s e tail))

    true (throw (RuntimeException. (str head " is not applicable!")))))

(deftracefn resolve [s e f]
  (let [e (env/merge-local e f)]
    (if (:inhibit? s)
      (ast/immediate f)
      (cond
        (ast/input? f)  (if (env/bound? e f)
                          (walk s e (env/binding e f))
                          (ast/immediate f))
        (ast/ref? f)    (:binding f)
        (ast/symbol? f) (ast/immediate f)
        true            (assert false "unreachable!!")))))

(deftracefn eval [s e f]
  (let [e (env/merge-local e f)]
    (cond
      (ast/coll? f)       (into (ast/empty f) (map #(walk s e (ast/immediate %))) f)
      (ast/pair? f)       (apply s e (walk s e (ast/immediate (:head f))) (:tail f))
      (ast/symbolic? f)   (resolve s e f)
      (ast/incomplete? f) (ast/immediate f)
      true                f)))

(deftracefn walk [s e f]
  (let [env (env/merge-local e f)]
    (cond
      (ast/immediate? f)   (eval s e (walk s e (:form f)))
      (ast/application? f) (apply s e (walk s e (:head f)) (:tail f))
      (ast/pair? f)        (env/with-env env
                             (let [s (-> s (assoc :μ? true) (assoc :inhibit? true))]
                               (-> f
                                   (update :head #(walk s {} %))
                                   (update :tail #(walk s {} %)))))
      (ast/symbolic? f)    (let [sym (ast/symbol f)]
                             (if (env/captured? s sym)
                               (ast/input {} sym (env/capid s sym))
                               (env/with-env env f)))
      (ast/coll? f)        (into (ast/empty f) (map (partial walk s e)) f)
      (ast/μ? f)           (let [s (-> s (env/uncapture (:param f)) (assoc :μ? true))]
                             (update f :body #(walk s (env/unbind e f) %)))
      true                 f)))
