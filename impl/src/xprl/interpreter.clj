(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]))

(declare walk)

(defn with-return [env rf f]
  (let [env (assoc env (ast/xkey :return) (ptask rf))]
    (f env)))

(defn return! {:style/indent 1} [env v]
  (ast/emission env [[(ast/xkey :return) v]]))

(defn call! [env f args]
  (ast/emission env [[:xprl.executor/new-task! [f args]]]))

(defn walk-emission [env em]
  (update em :msgs (fn [xs] (into [] (map (fn [[k v]] [(walk env k) v])) xs))))

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head)          (call! env head tail)
    (ast/external? head)   (call! env head tail)

    true (throw (RuntimeException. (str head " is not applicable!")))))

(deftracefn resolve [env f]
  (return! env
    (cond
      (ast/input? f)  (if (env/bound? f)
                        (walk env (env/binding f))
                        (ast/immediate f))
      (ast/ref? f)    (:binding f)
      (ast/symbol? f) (ast/immediate f)
      true            (assert false "unreachable!!"))))

(defn walk-coll [env f xs acc]
  (if (seq xs)
    (with-return env #(walk-coll env f (rest xs) (conj acc %))
      (walk env (f (first xs))))
    (return! env acc)))

(deftracefn eval [env f]
  (cond
    (ast/coll? f)       (walk-coll env ast/immediate f (ast/empty f))
    (ast/pair? f)       (with-return env #(apply env % (:tail f))
                          #(walk % (ast/immediate (:head f))))
    (ast/symbolic? f)   (resolve env f)
    true                (return! env f)))

(deftracefn walk* [env f]
  (cond
    (ast/immediate? f)   (with-return env #(eval env %) #(walk % (:form f)))
    (ast/application? f) (with-return env #(apply env % (:tail f)) #(walk % (:head f)))
    (ast/coll? f)        (walk-coll env identity f (ast/empty f))
    (ast/μ? f)           (return! env (update f :body walk))
    (ast/emission? f)    (return! env (walk-emission env f))
    true                 (return! env f)))

(def walk (memoize walk*))
