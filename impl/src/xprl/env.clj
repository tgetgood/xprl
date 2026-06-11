(ns xprl.env
  (:refer-clojure :exclude [bound? binding])
  (:require [clojure.set :as set]
            [xprl.ast :as ast]))

(defn bind [input val]
  (assoc input :binding val))

(defn bound? [input]
  (contains? input :binding))

(defn binding [form]
  (get form :binding))

(defmacro walk-cond
  "Separate tree traversal from the important logic."
  [form walk & cases]
  {:syle/indent [2]}
  `(cond
     (ast/immediate? ~form)   (update ~form :form ~walk)
     (ast/application? ~form) (-> ~form (update :head ~walk) (update :tail ~walk))
     (ast/pair? ~form)        (-> ~form (update :head ~walk) (update :tail ~walk))
     (ast/coll? ~form)        (into (ast/empty ~form) (map ~walk) ~form)
     (ast/emission? ~form)    (update ~form :msgs ~walk)
     ~@cases
     true                     ~form ))

;; REVIEW: Three separate tree walkers probably indicates something wrong.

(declare walk-capture)

(defn walk-capture* [sym input form]
  (let [walk (partial walk-capture sym input)]
    (walk-cond form walk
      ;; If we're creating nested μs from the outside in, then we'll need
      ;; to clobber captured inputs in narrower contexts.
      ;;
      ;; However, once a symbol is bound, that binding is permanent and it
      ;; cannot be recaptured. This should be obvious once you think it through,
      ;; but I've already had to think it through from scratch twice...
      (ast/input? form)    (if (bound? form)
                             (update form :binding walk)
                             (if (= (ast/symbol form) sym)
                               input
                               form))
      (ast/symbolic? form) (if (= (ast/symbol form) sym)
                             input
                             form)
      (ast/μ? form)        (if (or (= sym (:param form)) (= sym (:name form)))
                             form
                             (update form :body walk)))))

(def walk-capture (memoize walk-capture*))

(declare walk-bind)

(defn walk-bind* [bindings form]
  (let [walk (partial walk-bind bindings)]
    (walk-cond form walk
      (ast/input? form) (if (contains? bindings (:id form))
                          (bind form (get bindings (:id form)))
                          (if (bound? form)
                            (update form :binding walk)
                            form))
      (ast/μ? form)     (update form :body walk))))

(def walk-bind (memoize walk-bind*))

(defn walk-rename [find replace form]
  (let [walk (partial walk-rename find replace)]
    (walk-cond form walk
      (ast/input? form) (if (= find (:id form))
                          (ast/input (:sym form) replace)
                          (if (bound? form)
                            (update form :binding walk)
                            form))
      (ast/μ? form)     (if (or (= find (:id form)) (= find (:recid form)))
                          form
                          (update form :body walk)))))

(defn rename-inputs [bindings]
  (into {} (map (fn [[k v]] [k (gensym (str k "-"))])) bindings))

(defn invoke* [bindings form]
  (let [renames (rename-inputs bindings)
        binds   (into {} (map (fn [[k v]] [(get renames k) v])) bindings)]
    (->> (reduce (fn [acc [k v]] (walk-rename k v acc)) form renames)
         (walk-bind binds))))

(def invoke (memoize invoke*))
