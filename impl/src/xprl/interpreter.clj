(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]))

(declare walk)

(defn with-return [env rf f]
  (let [env (assoc env (ast/xkey :return) rf)]
    (f env)))

(defn return! {:style/indent 1} [env v]
  (if (ast/emission? v)
    v
    (ast/emission env [[(ast/xkey :return) v]])))

(defn walk-coll [env xs acc]
  (if (seq xs)
    (with-return env #(walk-coll env (rest xs) (conj acc %))
      #(walk % (first xs)))
    (return! env acc)))

;; REVIEW: What is the point of turning this into an emission?
;;
;; Yes, the fact that the message passing and invocation are isomorphic is of
;; theoretical interest. It's very pretty. But actually doing it seems to be
;; just unnecessary complexity.

#_(deftracefn apply [env head tail]
  (with-return env
    (cond
      (ast/μ? head)        (fn [tail]
                             (let [bindings {(:id head) tail, (:rec head) head}]
                               (walk env (env/invoke bindings (:body head)))))
      (ast/external? head) (fn [tail] (ast/call env head tail))

      true (throw (RuntimeException. (str head " is not applicable!"))))
    #(return! % tail)))

;; The equivalent version with fewer steps.
(deftracefn apply [env head tail]
  (cond
    (ast/μ? head)        (let [bindings {(:id head) tail, (:rec head) head}]
                           (walk env (env/invoke bindings (:body head))))
    (ast/external? head) (ast/call env head tail)

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

(deftracefn eval [env f]
  (cond
    (ast/coll? f)       (walk-coll env (map ast/immediate f) (ast/empty f))
    (ast/pair? f)       (with-return env #(apply env % (:tail f))
                          #(walk % (ast/immediate (:head f))))
    (ast/symbolic? f)   (resolve env f)
    true                (return! env f)))

(deftracefn walk* [env f]
  (cond
    (ast/immediate? f)   (with-return env #(eval env %) #(walk % (:form f)))
    (ast/application? f) (with-return env #(apply env % (:tail f)) #(walk % (:head f)))
    (ast/coll? f)        (walk-coll env f (ast/empty f))
    (ast/μ? f)           (return! env (update f :body walk))
    (ast/emission? f)    f
    true                 (return! env f)))

(def walk (memoize walk*))
