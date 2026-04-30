(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head) (let [env (-> env
                                (env/uncapture (:param head))
                                (env/bind (:id head) tail)
                                (env/bind (:rec head) (ast/recurser head)))]
                    (walk env (:body head)))

    (ast/external? head)   (ast/call env head tail)
    (ast/incomplete? head) (ast/application head (walk env tail))
    (ast/recurser? head)   (ast/application head tail)

    true (throw (RuntimeException. (str head " is not applicable!")))))

(deftracefn resolve [env f]
  (cond
    (ast/input? f)  (let [env (env/merge-local env f)]
                      (if (env/bound? env f)
                        (let [[env val] (env/binding env f)]
                          (walk (env/merge-envs (:env f) env) val))
                        (ast/immediate f)))
    (ast/ref? f)    (:binding f)
    (ast/symbol? f) (ast/immediate f)
    true            (assert false "unreachable!!")))

(deftracefn eval [env f]
  (cond
    (ast/coll? f)       (into (ast/empty f) (map #(walk env (ast/immediate %))) f)
    (ast/pair? f)       (apply env (walk env (ast/immediate (:head f))) (:tail f))
    (ast/symbolic? f)   (resolve env f)
    (ast/incomplete? f) (ast/immediate f)
    true                f))

(deftracefn walk [env f]
  (cond
    (ast/immediate? f)   (eval env (walk env (:form f)))
    (ast/application? f) (apply env (walk env (:head f)) (:tail f))
    (ast/pair? f)        (ast/pair (walk env (:head f)) (walk env (:tail f)))
    (ast/input? f)       (let [env (env/merge-local env f)]
                           ;; Once a parameter is bound, the surrounding env
                           ;; becomes important. But if it isn't bound yet, then
                           ;; the env can't effect anything it might later be
                           ;; bound to, can it?
                           ;; REVIEW: I'm not so sure.
                           (if (env/bound? env f) (env/with-env env f) f))
    (ast/symbolic? f)    (let [sym (ast/symbol f)]
                           (if (env/captured? env sym)
                             (ast/input {} sym (env/capid env sym))
                             f))
    (ast/coll? f)        (into (ast/empty f) (map (partial walk env)) f)
    ;; We don't want outer arguments to effect inner calls during recursion!
    ;; REVIEW: Is this a real problem, or am I chasing ghosts?
    (ast/μ? f)           (update f :body #(walk (env/unbind env f) %))
    true                 f))
