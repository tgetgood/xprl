(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]))

(defn error [& strs]
  (throw (RuntimeException. ^String (clojure.core/apply str strs))))

(def ret (ast/xkey :return))

(defn return {:style/indent 1} [env x]
  (let [retfn (get env ret)]
    (assert (fn? retfn) (str "Cannot return " x ". No destination."))
    ;; REVIEW: I skip the executor for local returns because it's simple and
    ;; I'm afraid of everything grinding to a halt if I don't. This could be
    ;; premature optimisation and I should look here first if there are weird
    ;; bugs.
    (retfn x)))

(defn with-return [env retfn]
  (assoc env ret retfn))

(defn ret-> {:style/indent [1]} [env inner outer]
  (inner (with-return env outer)))

(declare walk)

(defn walk-coll [env xs acc]
  (if (seq xs) ; FIXME: serial walk for simplicity. Do better. Eventually.
    (ret-> env #(walk % (first xs)) #(walk-coll env (rest xs) (conj acc %)))
    (return env acc)))

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
    (ast/pair? f)       (ret-> env
                          #(walk % (ast/immediate (:head f)))
                          #(apply env % (:tail f)))
    (ast/symbolic? f)   (resolve env f)
    (ast/incomplete? f) (return env (ast/immediate f))
    true                (return env f)) )

(deftracefn walk [env f]
  (cond
    (ast/immediate? f)   (ret-> env #(walk % (:form f)) #(eval env %))
    (ast/application? f) (ret-> env #(walk % (:head f)) #(apply env % (:tail f)))
    (ast/coll? f)        (walk-coll env f (ast/empty f))
    (ast/μ? f)           (ret-> env #(walk % (:body f)) #(return env (assoc f :body %)))
    (ast/emission? f)    (ret-> env #(walk % (:msgs f)) #(return env (assoc f :msgs %)))
    true                 (return env f))
  nil) ; make sure we can't accidentally rely on a return value
