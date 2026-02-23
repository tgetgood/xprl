(ns xprl.c2
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug :refer [deftracefn]]))

(declare walk)

(defn capture [env sym id]
  (assoc-in env [:captured sym] id))

(defonce μ       ; HACK: function equality is identity in clojure.
  (fn [env args] ; so DO NOT reload this, unless you reload everything
    (let [args (if (vector? args) args (walk env args))]
      (if (vector? args)
        (let [[param body] args
              param (walk env param)]
          (if (ast/symbolic? param)
            (let [id  (gensym "μ-param-")
                  env (capture env (ast/symbol param) id)]
              (ast/μ id param (walk env body)))
            (ast/application env μ [param body])))
        (ast/application env μ args)))))

(defn call [env f t]
  ((:fn f) env f t))

(defn apply-μ [env μ args]
  (assert false "not implemented"))

(defn bind [env id val]
  (assoc-in env [:bindings id] val))

(defn resolve [env form]
  (let [sym (ast/symbol form)]
    (cond
      (contains? (:captured env) sym) (ast/input sym (get-in env [:captured sym]))

      (ast/ref? form)    (:binding form)
      (ast/symbol? form) (ast/immediate form)
      true               (assert false "unreachable!!"))))

(defn apply [env head tail]
  (cond
    (ast/external? head)   (call env head (walk env tail))
    (ast/μ? head)          (walk (bind env (:id head) (walk env tail)) (:body head))
    ;; REVIEW: I don't like making μ this special, but I think I have to.
    (= μ head)             (μ env tail)
    (ast/incomplete? head) (ast/application env head (walk env tail))
    true                   (throw (RuntimeException. (str head " is not applicable!")))))

(defn eval [env form]
  (cond
    (ast/pair? form)       (apply env (walk env (ast/immediate (:head form))) (:tail form))
    (ast/symbolic? form)   (resolve env form)
    (ast/coll? form)       (into (empty form) (map (partial eval env)) form)
    (ast/incomplete? form) (ast/immediate form)
    true                   form))

(defn walk [env form]
  (cond
    (ast/immediate? form)   (eval env (walk env (:form form)))
    ;; REVIEW: `walk` can only hit a raw application if a previous pass hit a
    ;; wall and gave up. So we must assume we're operating with new information.
    ;; How do we reconcile the bundled env of the paused application with the
    ;; new evaluation env? I think stacking them like scheme is safe...
    (ast/application? form) (do (println "env: " (merge-with merge env (:env form)))
                                (apply (merge-with merge env (:env form))
                                       (walk env (:head form)) (:tail form)))
    (ast/input? form)       (if (contains? (:bindings env) (:id form))
                              (get-in env [:bindings (:id form)])
                              form)
    (ast/coll? form)        (into (empty form) (map (partial walk env)) form)
    true                    form))
