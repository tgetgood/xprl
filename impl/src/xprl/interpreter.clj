(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(defn walk-emission [env f]
  (let [env (env/merge-envs (:env f) env)]
    (-> (assoc f :env env)
        (update :msgs #(mapv (fn [[k v]] [(walk env k) v]) %)))))

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head)          (let [bindings {(:id head)  tail
                                           (:rec head) (ast/recurser head)}]
                             (walk env (env/walk-bind bindings (:body head))))
    (ast/external? head)   (ast/call env head tail)
    (ast/incomplete? head) (ast/application head (walk env tail))
    (ast/recurser? head)   (ast/emission env
                             [[(ast/xkey :return) (ast/application (:p head) tail)]])

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

(deftracefn walk [env f]
  (let [walk (partial walk env)]
    (cond
      (ast/immediate? f)   (eval env (walk (:form f)))
      (ast/application? f) (apply env (walk (:head f)) (:tail f))
      (ast/coll? f)        (into (ast/empty f) (map walk) f)
      (ast/μ? f)           (update f :body walk)
      (ast/emission? f)    (walk-emission env f)
      true                 f)))

;; TODO: current work list
;;
;; 1) reimplement (or restore) pipes as infinite lazy seqs of SVs.
;; 2) rewrite xprl.system as a message passing router using pipes instead of
;; svs/not-a-compiler as currently implemented.
;; 3) Extend the interpreter so that the rest of the tests pass
;; 4) squiggol impl
;; 5) implement :env channel and namespaces in xprl itself
;; 6) (might need to switch 5 & 6) figure out data representations in xprl
;; itself.
