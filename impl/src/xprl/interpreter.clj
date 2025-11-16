(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug :refer [deftracefn]]
   [xprl.env :as env]
   [xprl.system :as sys]))

(declare walk)

;;;;; Apply

(def *ev (atom nil))

(defn apply-error [app]
  (reset! *ev app)
  (throw (RuntimeException. (str (:head app) " is not applicable!\n" app))))

(deftracefn apply [{:keys [head tail] :as form} opts]
  (cond
    (ast/external? head) ((:fn head) form opts) ; Punt to external interpreter.
    (ast/μ? head)        (env/bindargs head tail)
    true                 (apply-error form)))

;;;;; Eval

(deftracefn eval [{form :form :as im} _] ; eval doesn't currently use any options
  (cond
    (ast/symbol? form) (env/resolve im)
    ;; (I (P x y)) => (A (I x) y)
    (ast/pair? form) (ast/application (ast/immediate (:head form)) (:tail form))
    ;; (I (L x y ...)) => (L (I x) (I y) ...)
    (vector? form)   (into [] (map ast/immediate) form)
    ;; (I {x y ...}) => {(I x) (I y) ...}
    ;; FIXME: maps are a pain in the ass because records are maps...
    (ast/map? form)  (into {} (map #(mapv ast/immediate %)) form)
    ;; (I V) => V. values are fixed points of eval.
    true             form))

;;;;; Walk (previously `reduce`)

(deftracefn walk [form {:keys [freeze?] :as opts}]
  (cond
    (ast/immediate? form)   (if (ast/incomplete? (:form form))
                              (let [form (update form :form walk opts)]
                                (if (ast/incomplete? (:form form))
                                  form
                                  (eval form opts)))
                              (eval form opts))
    (ast/application? form) (if (ast/incomplete? (:head form))
                              (let [form (update form :head walk opts)]
                                (if (ast/incomplete? (:head form))
                                  (update form :tail walk opts)
                                  (apply form opts)))
                              (apply form opts))
    (ast/emission? form)    (let [form (update form :kvs walk opts)]
                              (if freeze?
                                form
                                (sys/try-emissions! (update form :kvs walk opts))))

    (ast/ctx? form)  (sys/with-ctx (:chs form) (update form :form walk opts))
    (ast/μ? form)    (update form :body walk (assoc opts :freeze? true))
    (ast/pair? form) (-> form (update :head walk opts)
                         (update :tail walk opts))
    (ast/list? form) (into [] (map #(walk % opts)) form)
    (ast/map? form)  (into {} (map (fn [e] (mapv (fn [x] (walk x opts)) e))) form)
    (= :error form)  (throw (RuntimeException. "fatal error"))
    true             form))

;; Rewalk until fixed point. Is this really the best I can do?

(defn interpret [form]
  (loop [form form]
    (debug/trace! "start")
    (let [next (walk form {})]
      (if (= next form)
        form
        (recur next)))))
