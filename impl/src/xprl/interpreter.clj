(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug :refer [deftracefn]]
   [xprl.env :as env]
   [xprl.system :as sys]))

;;;;; Apply

(defn apply-error [app]
  (throw (RuntimeException. (str (:head app) " is not applicable!\n" app))))

(deftracefn apply [{:keys [head tail] :as form} env]
  (cond
    (ast/external? head) ((:fn head) form env) ; Punt to external interpreter.
    (ast/μ? head)        (env/bind head (env/anchor env tail))
    true                 (apply-error form)))

;;;;; Eval

(defn resolve [{sym :form :as im} env]
  (cond
    (env/bound? env sym) (env/resolve env sym)
    (ast/resolved? sym)  (:val sym)
    true                 im))

(deftracefn eval [{form :form :as im} env]
  (cond
    (ast/symbol? form) (resolve im env)
    ;; ;; (I (B X)) => (B (I X)) i.e. evaluation uses inner bindings
    ;; (ast/lex? form)    (ast/lex (:bindings form) (ast/immediate (:form form)))
    ;; (I (P x y)) => (A (I x) y)
    (ast/pair? form)   (ast/application (ast/immediate (:head form)) (:tail form))
    ;; (I (L x y ...)) => (L (I x) (I y) ...)
    (vector? form)     (into [] (map ast/immediate) form)
    ;; (I {x y ...}) => {(I x) (I y) ...}
    ;; FIXME: maps are a pain in the ass because records are maps...
    (ast/map? form)    (into {} (map #(mapv ast/immediate %)) form)
    ;; (I V) => V. values are fixed points of eval.
    true               form))

;;;;; Walk (previously `reduce`)

(defn try-emission!
  "Sends any messages that are ready to go, returns an emission containing the
  rest."
  [{:keys [kvs]} {:keys [ctx] :as env}]
  (ast/emission (loop [kvs kvs]
                  (if (seq kvs)
                    (let [[[k v] & more] kvs]
                      (if (ast/keyword? k)
                        (do
                          (sys/emit! ctx k (env/anchor env v))
                          (recur more))
                        kvs))
                    []))))

(deftracefn walk [form env]
  (cond
    (ast/immediate? form)   (cond
                              ;; we need this case to prevent a cycle when pushing
                              (ast/lex? (:form form))
                              (walk (ast/lex (:bindings (:form form))
                                             (ast/immediate (:form (:form form)))) env)
                              (ast/incomplete? (:form form))
                              (let [form (update form :form walk env)]
                                (if (ast/incomplete? (:form form))
                                  form
                                  (eval form env)))
                              true (eval form env))
    (ast/application? form) (if (ast/incomplete? (:head form))
                              (let [form (update form :head walk env)]
                                (if (ast/incomplete? (:head form))
                                  (update form :tail walk env)
                                  (apply form env)))
                              (apply form env))
    (ast/emission? form)    (let [form (update form :kvs walk env)]
                              (if (:μ? env)
                                form
                                (try-emission! (update form :kvs walk env) env)))

    ;; todo: i'll need a special node type for capture at this rate.
    (ast/lex? form)  (update form :form walk (env/incorporate env form))
    (ast/ctx? form)  (update form :form walk (env/walk-channels env form))
    (ast/μ? form)    (update form :body walk (env/walk-μ env form))
    (ast/pair? form) (-> form (update :head walk env) (update :tail walk env))
    (ast/list? form) (into [] (map #(walk % env)) form)
    (ast/map? form)  (into {} (map (fn [e] (mapv (fn [x] (walk x env)) e))) form)
    (= :error form)  (throw (RuntimeException. "fatal error"))
    true             form))

;; Well... Is it too simple now?

(defn interpret [form]
  (loop [f form]
    (debug/trace! "step")
    (let [next    (walk f env/empty-env)
          cleaned (env/pushdown next env/empty-env)]
      (debug/trace! "cleaned\n" next "\n–>\n" cleaned)
      (if (= f cleaned)
        f
        (recur cleaned)))))
