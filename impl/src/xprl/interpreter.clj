(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(deftracefn apply [s e head tail]
  (cond
    (ast/μ? head) (let [args (env/with-env (env/merge-local e tail) tail)]
                    (walk s (-> e
                                (env/merge-local head)
                                (env/uncapture (:param head))
                                (env/bind (:id head) args))
                          (:body head)))

    (ast/external? head)   (ast/call s e head tail)
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
      (ast/immediate? f)   (eval s env (walk s env (:form f)))
      (ast/application? f) (apply s e (walk s env (:head f)) (:tail f))
      (ast/pair? f)        (env/with-env env f
                             #_(-> f
                                 (update :head #(walk s {} %))
                                 ;; We only need to inhibit the tail
                                 (update :tail #(walk (assoc s :inhibit? true) {} %))))
      (ast/input? f)       (env/with-env env f)
      (ast/symbolic? f)    (let [sym (ast/symbol f)]
                             (if (env/captured? env sym)
                               (ast/input {} sym (env/capid env sym))
                               (env/with-env env f)))
      (ast/coll? f)        (into (ast/empty f) (map (partial walk s e)) f)
      (ast/emission? f)    (sys/try-emissions! s e (walk s e (:kvs f)))
      (ast/μ? f)           (update f :body #(walk (assoc s :μ? true) env %))
      true                 f)))
