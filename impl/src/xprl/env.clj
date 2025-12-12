(ns xprl.env
  (:refer-clojure :exclude [resolve extend])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace! record!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sym-walk
  "Replace every Symbol in `form` by a namespaced Ref (to `ns`)."
  [ns form]
  (if (and (ast/symbol? form) (contains? ns form))
    (ast/ref form (get ns form))
    (walk/walk (partial sym-walk ns) identity form)))

(def empty-ns {})

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/symbol? (keys ns)) ns)
  (sym-walk ns body))

(defn ns-intern [ns sym val]
  ;; REVIEW: Should it be an error to redefine symbols? Old uses of the symbol
  ;; will point to the old value, so previous code won't change. Without tooling
  ;; that will be mighty confusing, but without tooling this language will be
  ;; unusable.
  ;; (assert (not (contains? ns (ast/symbol sym))) "Symbols cannot be redefined.")
  (assoc ns (ast/symbol sym) val))

;; N.B.: This is used for tooling. Don't delete it.
(defn lookup [env sym]
  (assert (contains? env sym) sym)
  (get env sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; μ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare walk-replace)

;; Replacing symbols one at a time is inefficient, but so much clearer that for
;; now I'm sticking with it.
(defn walk-replace* [key val body]
  (cond
    (and (ast/μ? body) (or (= (:params body) (ast/symbol key))
                           (= (:name body) (ast/symbol key))))
    body
    (= body key)    val
    ;; Don't replace the :sym key of a Ref!!!
    (ast/ref? body) (update body :binding #(walk-replace key val %))
    true            (walk/walk (partial walk-replace key val) identity body)))

(def walk-replace (memoize walk-replace*))

(defn capture [body name uname]
  (walk-replace name uname body))

(defn bind [body [k v]]
  (walk-replace k (ast/ref k v) body))

(defn bindargs
  [{:keys [name params body] :as μ} args]
  (trace! "binding" (merge {params args} (when name {name μ})) "\nin\n" body)
  (assert (and (ast/symbol? params) (or (nil? name) (ast/symbol? name))))
  (let [m (merge {params args} (when name {name μ}))
        v (reduce bind body m)]
    (record! body v {:binding m})
    v))
