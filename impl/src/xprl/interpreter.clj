(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require [xprl.ast :as ast]
            [xprl.debug :refer [deftracefn]]
            [xprl.env :as env]))

(declare walk)

(defn with-return [env rf f]
  (let [env (assoc env (ast/xkey :return) (fn [arg] (fn [] (rf arg))))]
    (f env)))

(defn return! {:style/indent 1} [env v]
  (ast/emission env [[(ast/xkey :return) v]]))

(defn walk-coll [env xs acc]
  (if (seq xs)
    (with-return env #(walk-coll env (rest xs) (conj acc %))
      #(walk % (first xs)))
    (return! env acc)))

(defn walk-emission [env em]
  ;; FIXME: This should be handled by builtins/emit! `walk` shouldn't need to
  ;; worry about Emission records.
  ;;
  ;; REVIEW: What about emissions buried in μs? Will the capture/bind walkers
  ;; handle this properly? What about something like `(μ x (emit ~(f x) 42))` ?
  ;;
  ;; That will break without Emission walking until we bring back reactive inputs
  ;;
  ;; There are a lot of moving parts here and I don't want to mash the gears
  ;; ...again...
  (update em :msgs
          (fn [xs] (walk-coll env (map (fn [[k v]] [(walk env k) v]) xs) []))))

(deftracefn apply [env head tail]
  (with-return env
    (cond
      (ast/μ? head)        (fn [tail]
                             (let [bindings {(:id head) tail, (:rec head) head}]
                               (walk env (env/invoke bindings (:body head)))))
      (ast/external? head) (fn [tail] (ast/call env head tail))

      true (throw (RuntimeException. (str head " is not applicable!"))))
    #(return! % tail)))

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
    ;; (ast/emission? f)    (walk-emission env f)
    (ast/μ? f)           (return! env (update f :body walk))
    true                 (return! env f)))

(def walk (memoize walk*))
