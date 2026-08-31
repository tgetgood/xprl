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
    ;; Using transients here just feels sloppy. It does error out in
    ;; use-after-free type scenarios which is pretty valuable.
    (let [acc (transient acc)]
      (exec/enqueue!
       (exec/task env
         (fn [env] (cont/ret-> env #(walk % (first xs)) #(do (conj! acc %) nil)))
         (fn [env] (walk-coll env (rest xs) (persistent! acc))))))))

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
                           ;; Is merging the envs necessary? Is it even desirable?
                           ;; I suspect no on both counts, but a conclusive
                           ;; experiment/argument eludes me.
                           ;; If channel reroutes become part of the ast, then
                           ;; I'm almost certain it's just a source of bugs.
                           #(emit/do-emission! env #_(merge env (:env f)) %))
    (ast/route? f)       (ret-> env
                           #(walk % (:chmap f))
                           #(if (ast/incomplete? %)
                              (return env (assoc f :chmap %))
                              (walk (merge env %) (:body f))))
    true                 (return env f))
  nil) ; make sure we can't accidentally rely on a return value
