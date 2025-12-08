(ns xprl.interpreter
  (:refer-clojure :exclude [resolve eval apply])
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug :refer [deftracefn]]
   [xprl.env :as env]
   [xprl.system :as sys]))

(declare walk)

;;;;; Apply

(defn apply-error [app]
  (throw (RuntimeException. (str (:head app) " is not applicable!\n" app))))

(deftracefn apply [{:keys [head tail] :as form} opts]
  (cond
    (ast/external? head) ((:fn head) form opts) ; Punt to external interpreter.
    (ast/μ? head)        (let [tail (if (ast/incomplete? tail) (walk tail opts) tail)
                               form (assoc form :tail tail)]
                           (if (and (= head tail) (not (:rec? opts)))
                             form
                             (env/bindargs head tail)))
    true                 (apply-error form)))

;;;;; Eval

(defn resolve [{sym :form :as im} opts]
  (cond
    (ast/symbol? sym) im
    (ast/ref? sym)    (:binding sym)
    true              (assert false "unreachable!")))

(deftracefn eval [{form :form :as im} opts]
  (cond
    (ast/symbolic? form) (resolve im opts)
    ;; (I (P x y)) => (A (I x) y)
    (ast/pair? form)     (ast/application (ast/immediate (:head form)) (:tail form))
    ;; (I (L x y ...)) => (L (I x) (I y) ...)
    (vector? form)       (into [] (map ast/immediate) form)
    ;; (I {x y ...}) => {(I x) (I y) ...}
    ;; FIXME: maps are a pain in the ass because records are maps...
    (ast/map? form)      (into {} (map #(mapv ast/immediate %)) form)
    ;; (I V) => V. values are fixed points of eval.
    true                 form))

;;;;; Walk (previously `reduce`)

(deftracefn walk [form opts]
  (sys/with-return-ctx opts
    (cond
      (ast/immediate? form)   (if (ast/incomplete? (:form form))
                                (let [form (update form :form walk opts)]
                                  (if (ast/incomplete? (:form form))
                                    form
                                    (eval form opts)))
                                (eval form opts))
      (ast/application? form) (if (ast/incomplete? (:head form))
                                ;; FIXME: This `rec?` business is a kludge
                                ;; because I don't have a general stopping
                                ;; condition by which to detect when I'm in an
                                ;; infinite regress. It's entirely possible that
                                ;; I'm tripping on the halting problem, which is
                                ;; just peachy, but let's verify that before we
                                ;; panic...
                                (let [rec? (not (ast/incomplete? (:tail form)))
                                      form (update form :head walk (assoc opts :rec? rec?))]
                                  (if (ast/incomplete? (:head form))
                                    (update form :tail walk opts)
                                    (apply form opts)))
                                (apply form opts))

      (ast/emission? form) (sys/try-emissions! (update form :kvs walk opts) opts)
      (ast/ctx? form)      (sys/walk-ctx form
                             (update form :form walk (assoc opts :return-ctx? true)))

      ;; Pairs are data! walking them can lead to evaluation out of context!
      ;; But then μs are also data, so why do we walk them? just an optimisation?
      (ast/μ? form)    (update form :body walk (assoc opts :freeze? true))
      (ast/list? form) (into [] (map #(walk % opts)) form)
      (ast/map? form)  (into {} (map (fn [e] (mapv (fn [x] (walk x opts)) e))) form)
      (= :error form)  (throw (RuntimeException. "fatal error"))
      true             form)))

;; Rewalk until fixed point. Is this really the best I can do?

(defn interpret [form]
  (loop [form form]
    (debug/trace! "start")
    (let [next (walk form {})]
      (if (= next form)
        form
        (recur next)))))
