(ns xprl.compiler
  (:refer-clojure :exclude [eval apply resolve compile]))

(defn vjoiner [len next]
  (atom {:vec (into [] (take len (repeat nil)))
         :next next}))

(defn ready? [j]
  (not-any? nil? @j))

(defn join! [j i v]
  (let [res (swap! j update :vec assoc i v)]
    (when (ready? (:vec res))
      (:next ))))

(defn action [next]
  ;; next is a join location [joiner index].
  {:next next})

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
(defn compile [form env next]
  (cond
    (ast/immediate? form)   (eval form env next)
    (ast/application? form) (apply form env next)
    :else                   form))


{:head 'μ
 :tail '[~(nth* ~x 1) ~(nth* ~x 2)]}
