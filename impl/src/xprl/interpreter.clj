(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]))

(declare walk)

(defn walk-emission [env msgs]
  (into [] (map (fn [[k v]] [(walk env k) v])) msgs))

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head)          (let [bindings {(:id head) tail, (:rec head) head}]
                             (walk env (env/invoke bindings (:body head))))
    (ast/external? head)   (ast/call env head tail)
    (ast/incomplete? head) (ast/application head tail)
    true                   (throw (RuntimeException. (str head " is not applicable!")))))

(deftracefn resolve [env f]
  (cond
    (ast/input? f)  (if (env/bound? f)
                      (walk env (env/binding f))
                      (ast/immediate f))
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

(deftracefn walk* [env f]
  (cond
    (ast/immediate? f)   (eval env (walk env (:form f)))
    (ast/application? f) (apply env (walk env (:head f)) (:tail f))
    (ast/coll? f)        (into (ast/empty f) (map (partial walk env)) f)
    (ast/μ? f)           (update f :body (partial walk env))
    (ast/emission? f)    (update f :msgs (partial walk-emission env))
    true                 f))

(def walk (memoize walk*))
