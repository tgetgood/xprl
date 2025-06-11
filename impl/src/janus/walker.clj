(ns janus.walker
  "Overgeneralised. I thought I needed it, but I don't. And now I can't be
  bothered degeneralising it."
  (:refer-clojure :exclude [type])
  (:require
   [janus.ast :as ast]
   [janus.debug :as debug :refer [trace!]]
   [janus.env :as env]))

(defn rule-tree [rules]
  (reduce (fn [acc [k v]]
            (assoc-in acc (if (vector? k) (conj k :fn) [k :fn]) v))
          {} rules))

(defn step [x]
  (cond
    (instance? janus.ast.Immediate x)   (:form x)
    (instance? janus.ast.Application x) (:head x)
    true                                nil))

(defn type [types x]
  (get types (clojure.core/type x) :V))

(defn rule-match [rules types sexp]
  (let [t1 (type types sexp)]
    (if-let [subtree (get rules t1)]
      (let [subexp (step sexp)
            t2 (type types subexp)]
        (if-let [subsubtree (get subtree t2)]
          [[t1 t2] (:fn subsubtree)]
          [t1 (:fn subtree)]))
      [t1 identity])))

(defn trace-env [sexp]
  (ast/symbols sexp))

(defn walk-step [rules types]
  (let [rules (rule-tree rules)]
    (fn [sexp]
      (let [[rule f] (rule-match rules types sexp)]
        (trace! "rule match:" rule sexp "\n  syms:" (trace-env sexp))
        (let [v (f sexp)]
          (trace! "result:" rule "\n" sexp "\n->\n" v)
          [rule v])))))

(defn walk-to-fp
  "Walks form and rewalks result until we hit a fixed point x = (walk1 x).
  Maintains chain of provenance via metadata."
  ([walk1 env sexp]
   (walk-to-fp walk1 (env/pin sexp env)))
  ([walk1 sexp]
   (let [[rule v] (walk1 sexp)]
     (if (= v sexp)
       sexp
       (recur walk1 (debug/tag v rule sexp))))))
