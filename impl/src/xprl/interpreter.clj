(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]))

(declare walk)

(defn emit! [env f args]
  (ast/emission env [[:xprl.executor/new-task! [f args]]]))

(defn walk-emission [env em]
  (update em :msgs (fn [xs] (into [] (map (fn [[k v]] [(walk env k) v])) xs))))

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head)          (emit! env head tail)
    (ast/external? head)   (emit! env head tail)
    (ast/incomplete? head) (ast/application head (walk env tail))

    true (throw (RuntimeException. (str head " is not applicable!")))))

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
  (let [walk (partial walk env)]
    (cond
      (ast/immediate? f)   (eval env (walk (:form f)))
      (ast/application? f) (apply env (walk (:head f)) (:tail f))
      (ast/coll? f)        (into (ast/empty f) (map walk) f)
      (ast/μ? f)           (update f :body walk)
      (ast/emission? f)    (walk-emission env f)
      true                 f)))

(def walk (memoize walk*))
