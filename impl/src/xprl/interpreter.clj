(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]
            [xprl.system :as sys]))

(declare walk)

(defn softwalk [env f]
  (let [softwalk (partial softwalk env)]
    (cond
      (ast/immediate? f)   (update f :form softwalk)
      (ast/application? f) (-> f (update :head softwalk) (update :tail softwalk))
      (ast/pair? f)        (-> f (update :head softwalk) (update :tail softwalk))
      (ast/input? f)       f
      (ast/symbolic? f)    (walk env f)
      (ast/μ? f)           (update f :body softwalk)
      (ast/emission? f)    (update f :msgs softwalk)
      true                 f)))

(defn walk-emission [env f]
  (let [env (env/merge-envs (:env f) env)]
    (-> (assoc f :env env)
        (update :msgs #(mapv (fn [[k v]] [(walk env k) (walk env v)]) %)))))

(deftracefn apply [env head tail]
  (cond
    (ast/μ? head) (let [env (-> env
                                (env/uncapture (:param head))
                                (env/bind (:id head) tail)
                                (env/bind (:rec head) (ast/recurser head)))]
                    (walk env (:body head)))

    (ast/external? head)   (ast/call env head tail)
    (ast/incomplete? head) (ast/application head (walk env tail))
    (ast/recurser? head)   (ast/emission env
                             [[(ast/xkey :return) (ast/application (:p head) tail)]])

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
  (let [walk (partial walk env)]
    (cond
      (ast/immediate? f)   (eval env (walk (:form f)))
      (ast/application? f) (apply env (walk (:head f)) (:tail f))
      ;; (ast/pair? f)        (ast/pair (walk (:head f)) (softwalk env (:tail f)))
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
