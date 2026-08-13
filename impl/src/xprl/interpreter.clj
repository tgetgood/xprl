(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.continuation :as cont :refer [return ret-> with-return]]
            [xprl.debug :refer [deftracefn]]
            [xprl.emission :as emit]
            [xprl.env :as env]
            [xprl.executor :as exec]))

(defn error [& strs]
  (throw (RuntimeException. ^String (clojure.core/apply str strs))))

(declare walk)

(defn walk-coll [env xs acc]
  (if (empty? xs)
    (return env acc)
    (let [acc (transient acc)]
      (exec/on-complete! (fn [] (walk-coll env (rest xs) (persistent! acc))))
      (cont/ret-> env #(walk % (first xs)) #(do (conj! acc %) nil)))))

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head)          (ret-> env
                             #(walk % tail)
                             #(let [bindings {(:id head) % (:rec head) head}]
                                (walk env (env/invoke bindings (:body head)))))
    (ast/external? head)   (ast/call env head tail)
    (ast/incomplete? head) (return env (ast/application head tail))
    true                   (error head " is not applicable!"))
  nil)

(deftracefn resolve [env f]
  (if (ast/bound? f)
    (walk env (:binding f))
    (return env ; We *could* just wrap everything in `return`, technically...
      (cond
        (ast/ref? f)      (:binding f)
        (ast/captured? f) (ast/immediate f)
        (ast/symbol? f)   (ast/immediate f)
        true              (error "unreachable!!"))))
  nil)

(deftracefn eval [env f]
  (cond
    (ast/coll? f)       (walk-coll env (map ast/immediate f) (ast/empty f))
    (ast/pair? f)       (walk env (ast/application (ast/immediate (:head f)) (:tail f)))
    (ast/symbolic? f)   (resolve env f)
    (ast/incomplete? f) (return env (ast/immediate f))
    true                (return env f))
  nil)

(deftracefn walk [env f]
  (assert (not (empty? env)))
  (cond
    (ast/immediate? f)   (ret-> env #(walk % (:form f)) #(eval env %))
    (ast/application? f) (ret-> env #(walk % (:head f)) #(apply env % (:tail f)))
    (ast/coll? f)        (walk-coll env f (ast/empty f))
    (ast/μ? f)           (ret-> (emit/cut env ::test)
                           #(walk % (:body f))
                           #(return env (assoc f :body %)))
    (ast/emission? f)    (ret-> env
                           #(walk % (:msgs f))
                           #(emit/do-emission! (merge env (:env f)) %))
    (ast/net? f)         (let [w    (emit/wire)
                               env' (with-return (:env f) w)]
                           (exec/enqueue-all! (map #(with-meta (fn [] (walk env %))
                                                      {:form %}) (:forms f)))
                           (return env w))
    true                 (return env f))
  nil) ; make sure we can't accidentally rely on a return value
