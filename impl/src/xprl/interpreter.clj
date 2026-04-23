(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(deftracefn apply [e head tail]
  (cond
    (ast/μ? head) (let [args (env/with-env (env/merge-local e tail) tail)]
                    (walk (-> e
                              (env/merge-local head)
                              (env/uncapture (:param head))
                              (env/bind (:id head) args))
                          (:body head)))

    (ast/external? head)   (ast/call e head tail)
    (ast/incomplete? head) (ast/application head (walk e tail))

    true (throw (RuntimeException. (str head " is not applicable!")))))

(deftracefn resolve [e f]
  (let [e (env/merge-local e f)]
    (cond
      (ast/input? f)  (if (env/bound? e f)
                        (walk e (env/binding e f))
                        (ast/immediate f))
      (ast/ref? f)    (:binding f)
      (ast/symbol? f) (ast/immediate f)
      true            (assert false "unreachable!!"))))

(deftracefn eval [e f]
  (let [e (env/merge-local e f)]
    (cond
      (ast/coll? f)       (into (ast/empty f) (map #(walk e (ast/immediate %))) f)
      (ast/pair? f)       (apply e (walk e (ast/immediate (:head f))) (:tail f))
      (ast/symbolic? f)   (resolve e f)
      (ast/incomplete? f) (ast/immediate f)
      true                f)))

(deftracefn walk [e f]
  (let [env (env/merge-local e f)]
    (cond
      (ast/immediate? f)   (eval env (walk env (:form f)))
      ;; TODO: Smoking gun: switching `e` to `env` in `apply` below causes a
      ;; stack overflow when walking `if`. That might be why if doesn't work,
      ;; but I suspect it's just another bug.
      (ast/application? f) (apply e (walk env (:head f)) (:tail f))
      (ast/pair? f)        (env/with-env env f)
      (ast/input? f)       (env/with-env env f)
      (ast/symbolic? f)    (let [sym (ast/symbol f)]
                             (if (env/captured? env sym)
                               (ast/input {} sym (env/capid env sym))
                               f))
      (ast/coll? f)        (into (ast/empty f) (map (partial walk env)) f)
      (ast/μ? f)           (update f :body #(walk env %))
      true                 f)))
