(ns xprl.builtins
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.ns :as ns]
   [xprl.system :as sys]))

;;;;; simple primitive fns

(defn call-primitive-fn
  "given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (fn [env head tail]
    (if (ast/incomplete? tail)
      (ast/application env head tail)
      (try
        (apply f tail)
        (catch Exception e
          (reset! debug/*pfn {:f f :args tail :e e})
          (binding [ast/*verbose* true]
            (ast/inspect (ast/application f tail)))
          (println e)
          :error)))))

(defn primitive [n f]
  (ast/extern n (call-primitive-fn f)))

(defn primitives [m]
  (reduce (fn [acc [k v]] (assoc acc (ast/symbol k) (primitive k v))) {} m))

(defn rest* [xs]
  (into [] (rest xs)))

(defn empty?* [x]
  (boolean (empty? x)))

(defn not* [x]
  (assert (boolean? x))
  (not x))

(def fns
  (primitives
   {"+*"   +
    "**"   *
    "-*"   -
    "/*"   /
    ">*"   >
    "<*"   <
    "=*"   =
    "mod*" mod
    "not*" not*
    "str*" str

    "list?*"  ast/list?
    "map?*"   ast/map?
    "dot?*"    ast/dot?
    "string?*" string?

    "merge*"  merge
    "empty?*" empty?*
    "get*"    get

    ;; REVIEW: All symbols in a form are converted to Refs on read. The only
    ;; time we get raw Symbols back is when a μ shadows a Ref with its
    ;; parameter. So when we say `symbol?` in xprl we could mean either. But
    ;; then should they be distinguishable in the language?
    "symbol?*" ast/symbolic?

    "first*" first
    "rest*"  rest*

    "count*" count
    }))

;; nth can operate on a vector even if the elements of that vector cannot yet be
;; computed. This is such an important simplification that I'm willing to stick
;; in a kludge like this.
(defn nth* [env self args]
  (let [args (if (vector? args) args (i/walk env args))]
    (if (and (vector? (first args)) (int? (second args)))
      (nth (first args) (dec (second args))) ; base 1 indexing
      (ast/application env self args))))

(def direct-externs
  {(ast/symbol "nth*") (ast/extern "nth*" nth*)})

;;;;; specialish forms

(defn emit [env self kvs]
  (let [kvs (if (vector? kvs) kvs (i/walk env kvs))]
    (if (vector? kvs)
      (do (assert (even? (count kvs)))
          (let [msgs (i/walk env (mapv (fn [[k v]] [(ast/immediate k) v]) (partition 2 kvs)))]
            (sys/try-emissions! env msgs)))
      (ast/application env self kvs))))

#_(defn with-channels [{[chmap body] :tail :as app} opts]
  (if (ast/incomplete? chmap)
    (update app :tail i/walk opts)
    (ast/ctx chmap body)))

(defn μ [env self args]
  (let [args (if (vector? args) args (i/walk env args))]
    (if (vector? args)
      (let [[param body] args
            param        (i/walk env param)]
        (if (ast/symbolic? param)
          (let [id    (gensym "μ-param-")
                param (ast/symbol param)
                env   (env/capture env param id)]
            (ast/μ env id param (i/walk env body)))
          (ast/application env self [param (i/walk env body)])))
      (ast/application env self args))))


(defn macros [m]
  (reduce (fn [acc [k f]]
            (assoc acc (ast/symbol k) (ast/macro k f))) {} m))

(def special
  "things that would traditionally be special forms."
  (macros
   {"μ"    μ
    "emit" emit
    ;; "with-channels" with-channels

    ;; TODO: builtin macros needed for a working system.
    ;;
    ;; pipe
    ;; net
    ;;
    ;; TODO: These operators will likely need to be converted from plain old
    ;; functions.
    ;;
    ;; "first*" first*
    ;; "rest*"  rest*
    }))

;;;;; The Ur context from which all programs derive.
;;
;; I don't really like this being so ad hoc. There will have to be a takeover
;; moment when the intended long term context and history system is finally
;; built. that is to say there will be a shock in the history where we suddenly
;; have no past, no origin. why is bootstrapping so singular like that?

(def base-env
  (reduce (fn [e [k v]] (ns/ns-intern e k v)) ns/empty-ns (merge direct-externs special fns)))
