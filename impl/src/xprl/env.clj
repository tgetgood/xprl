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
     (ast/net? ~form)         (update ~form :forms ~walk)
     ~@cases
     true                     ~form ))

;; REVIEW: Three separate tree walkers probably indicates something wrong.
;;
;; And yet they're so subtley different that any attempt to unify them
;; introduces bugs. This is simple enough so why fuck with it?

(defn walk-capture [captures form]
  (let [walk (partial walk-capture captures)
        sym  (when (ast/symbolic? form) (ast/symbol form))]
    (walk-cond form walk
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

(defn walk-rename [smap form]
  (let [walk (partial walk-rename smap)]
    (walk-cond form walk
      (ast/bound? form)    (update form :binding walk)
      (ast/captured? form) (if (contains? smap (:id form))
                             (ast/capture (:sym form) (get smap (:id form)))
                             form)
      ;; Don't forget to stop when we hit a recursive call to the same fn!
      (ast/μ? form)        (let [smap (dissoc smap (:id form) (:recid form))]
                             (if (empty? smap)
                               form
                               (update form :body (partial walk-rename smap)))))))

(defn rename-inputs [bindings]
  (into {} (map (fn [[k v]] [k (gensym (str k "-"))])) bindings))

(defn invoke [bindings form]
  (let [renames (rename-inputs bindings)
        binds   (into {} (map (fn [[k v]] [(get renames k) v])) bindings)]
    (->> form
         (walk-rename renames)
         (walk-bind binds))))
