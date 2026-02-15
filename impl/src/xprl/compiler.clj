(ns xprl.compiler
  (:refer-clojure :exclude [eval apply resolve compile]))


(defn application )

(defn apply
  ([form env] (apply (:head form) (:tail form) env))
  ([head tail env]
   (cond
     (ast/external? head) (head form env)
     (ast/μ? head)        (compile-μ-apply head tail env)
     :else                (throw (Exception. (str "bad application:" head "," tail))))))

(defn eval [form env]
  (cond
    (ast/symbolic? form) (resolve form env)
    (ast/pair? form)     (compile (ast/immediate (:head form)) env
                                  {:call :apply
                                   :args (:tail env)
                                   :env  env})))

;; this is going to be a fairly standard stacked env interpreter which
;; interprets forms into an intermediate representation. So a compiler... more
;; or less.
(defn compile [form state]
  (cond
    (ast/immediate? form)   (eval form state)
    (ast/application? form) (apply form state)
    :else                   form))

(defn createμ [form env conts])
