(ns xprl.env
  (:require [clojure.set :as set]
            [xprl.ast :as ast]))

(defmacro walk-cond
  "Separate tree traversal from the important logic."
  [form walk & cases]
  ;; FIXME: I don't think this can work...
  {:syle/indent [[:form :form] :cond]}
  `(cond
     (ast/immediate? ~form)   (update ~form :form ~walk)
     (ast/application? ~form) (-> ~form (update :head ~walk) (update :tail ~walk))
     (ast/pair? ~form)        (-> ~form (update :head ~walk) (update :tail ~walk))
     (ast/coll? ~form)        (into (ast/empty ~form) (map ~walk) ~form)
     (ast/emission? ~form)    (update ~form :msgs ~walk)
     ~@cases
     true                     ~form ))

;; REVIEW: Three separate tree walkers probably indicates something wrong.

(defn walk-capture [sym input form]
  (let [walk (partial walk-capture sym input)]
    (walk-cond form walk
      ;; If we're creating nested μs from the outside in, then we'll need
      ;; to clobber captured inputs in narrower contexts.
      ;;
      ;; However, once a symbol is bound, that binding is permanent and it
      ;; cannot be recaptured. This should be obvious once you think it through,
      ;; but I've already had to think it through from scratch twice...
      (ast/captured? form) (if (= (ast/symbol form) sym)
                                input
                                form)
      (ast/bound? form)    (update form :binding walk)
      (ast/symbolic? form) (if (= (ast/symbol form) sym)
                             input
                             form)
      (ast/μ? form)        (if (or (= sym (:param form)) (= sym (:name form)))
                             form
                             (update form :body walk)))))

(defn walk-bind [bindings form]
  (let [walk (partial walk-bind bindings)]
    (walk-cond form walk
      (ast/bound? form)    (update form :binding walk)
      (ast/captured? form) (if (contains? bindings (:id form))
                             (ast/bind form (get bindings (:id form)))
                             form)
      (ast/μ? form)        (update form :body walk))))

(defn walk-rename [find replace form]
  (let [walk (partial walk-rename find replace)]
    (walk-cond form walk
      (ast/bound? form)    (update form :binding walk)
      (ast/captured? form) (if (= find (:id form))
                             (ast/capture (:sym form) replace)
                             form)
      (ast/μ? form)        (if (or (= find (:id form)) (= find (:recid form)))
                          form
                          (update form :body walk)))))

(defn rename-inputs [bindings]
  (into {} (map (fn [[k v]] [k (gensym (str k "-"))])) bindings))

(defn invoke [bindings form]
  (let [renames (rename-inputs bindings)
        binds   (into {} (map (fn [[k v]] [(get renames k) v])) bindings)]
    (->> (reduce (fn [acc [k v]] (walk-rename k v acc)) form renames)
         (walk-bind binds))))

(defn fixed? [form]
  (cond
    (ast/bound? form)       true
    (ast/ref? form)         true
    (ast/symbol? form)      true
    (ast/captured? form)    false
    (ast/immediate? form)   (fixed? (:form form))
    (ast/application? form) (and (fixed? (:head form)) (fixed? (:tail form)))
    (ast/pair? form)        (and (fixed? (:head form)) (fixed? (:tail form)))
    (ast/μ? form)           (fixed? (:body form))
    (ast/emission? form)    (fixed? (:msgs form))
    (ast/coll? form)        (every? fixed? form)
    true                    true))
