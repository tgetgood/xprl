(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.emission :as emit :refer [return ret->]]
            [xprl.env :as env]))

(defn error [& strs]
  (throw (RuntimeException. ^String (clojure.core/apply str strs))))

(declare walk)

(defn walk-coll [env xs acc]
  (let [acc (transient acc)]
    (when (seq xs)
      (loop [[x & xs] xs]
        (ret-> env #(walk % x) #(conj! acc %))
        (when (seq xs)
          (recur xs))))
    (return env (persistent! acc))))

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head)          (ret-> env
                             #(walk % tail)
                             #(let [bindings {(:id head) % (:rec head) head}]
                                (walk env (env/invoke bindings (:body head)))))
    (ast/external? head)   (ast/call env head tail)
    (ast/incomplete? head) (return env (ast/application head tail))
    true                   (error head " is not applicable!")))

(deftracefn resolve [env f]
  (if (ast/bound? f)
    (walk env (:binding f))
    (return env ; We *could* just wrap everything in `return`, technically...
      (cond
        (ast/ref? f)      (:binding f)
        (ast/captured? f) (ast/immediate f)
        (ast/symbol? f)   (ast/immediate f)
        true              (error "unreachable!!")))))

(deftracefn eval [env f]
  (cond
    (ast/coll? f)       (walk-coll env (map ast/immediate f) (ast/empty f))
    (ast/pair? f)       (walk env (ast/application (ast/immediate (:head f)) (:tail f)))
    (ast/symbolic? f)   (resolve env f)
    (ast/incomplete? f) (return env (ast/immediate f))
    true                (return env f)))

(deftracefn walk [env f]
  (assert (not (empty? env)))
  (cond
    (ast/immediate? f)   (ret-> env #(walk % (:form f)) #(eval env %))
    (ast/application? f) (ret-> env #(walk % (:head f)) #(apply env % (:tail f)))
    (ast/coll? f)        (walk-coll env f (ast/empty f))
    (ast/μ? f)           (ret-> (emit/cut env ::test)
                           #(walk % (:body f))
                           #(return env (assoc f :body %)))
    (ast/emission? f)    (ret-> env #(walk % (:msgs f)) #(emit/do-emission! env f %))
    true                 (return env f))
  nil) ; make sure we can't accidentally rely on a return value
