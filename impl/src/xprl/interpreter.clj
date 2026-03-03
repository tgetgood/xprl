(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(deftracefn apply [s e head tail]
  (cond
    (ast/μ? head) (let [args (walk s e tail)]
                    (if (and (:μ? s) (= args head))
                      ;; delay applying a μ to itself until it reaches the root context.
                      (ast/application e head tail)
                      (walk s (env/bind (env/merge-local e head) (:id head) args)
                            (:body head))))

    (ast/external? head)   (ast/call s e head tail)
    (ast/incomplete? head) (ast/application e head (walk s e tail))

    true (throw (RuntimeException. (str head " is not applicable!")))))

(deftracefn resolve [s e f]
  (let [e (env/merge-local e f)]
    (cond
      (ast/input? f)  (if (env/bound? e f)
                        (walk s (env/unbind e f) (env/binding e f))
                        (ast/immediate f))
      (ast/ref? f)    (:binding f)
      (ast/symbol? f) (ast/immediate f)
      true            (assert false "unreachable!!"))))

(deftracefn eval [s e f]
  (let [e (env/merge-local e f)]
    (cond
      (ast/coll? f) (into (ast/empty f) (map #(walk s e (ast/immediate %))) f)
      (ast/pair? f) (apply s e (walk s e (ast/immediate (:head f))) (:tail f))

      (ast/symbolic? f)   (resolve s e f)
      (ast/incomplete? f) (ast/immediate f)
      true                f)))

(deftracefn walk [s e f]
  (let [env (env/merge-local e f)]
    (cond
      (ast/immediate? f)   (eval s e (walk s e (:form f)))
      (ast/application? f) (apply s e (walk s e (:head f)) (:tail f))
      (ast/pair? f)        (env/with-env f env)
      (ast/input? f)       (env/with-env f env)
      (ast/coll? f)        (into (ast/empty f) (map (partial walk s e)) f)
      (ast/emission? f)    (sys/try-emissions! s e (walk s e (:kvs f)))

      (ast/μ? f) (update f :body #(walk {:μ? true} (env/unbind e f) %))
      true       f)))
