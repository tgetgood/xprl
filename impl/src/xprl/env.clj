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

(defn walk-capture [captures form]
  (let [walk (partial walk-capture captures)
        sym  (when (ast/symbolic? form) (ast/symbol form))]
    (walk-cond form walk
      ;; If we're creating nested μs from the outside in, then we'll need
      ;; to clobber captured inputs in narrower contexts.
      ;;
      ;; However, once a symbol is bound, that binding is permanent and it
      ;; cannot be recaptured. This should be obvious once you think it through,
      ;; but I've already had to think it through from scratch twice...
      (ast/bound? form)    (update form :binding walk)
      (ast/symbolic? form) (if (contains? captures sym)
                                (get captures sym)
                                form)
      (ast/μ? form)        (let [caps (dissoc captures (:param form) (:name form))]
                             (if (empty? caps)
                               form
                               (update form :body (partial walk-capture caps)))))))

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
