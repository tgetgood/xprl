(ns xprl.builtins
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]))

;;;;; simple primitive fns

(defn call-primitive-fn
  "given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (fn [{tail :tail :as form} opts]
    (let [{:keys [tail] :as next} (if (or (ast/incomplete? tail) (some ast/incomplete? tail))
                                    (update form :tail i/walk opts)
                                    form)]
      (if (or (ast/incomplete? tail) (some ast/incomplete? tail))
        next
        (try
          (apply f tail)
          (catch Exception e
            (reset! debug/*pfn {:f f :args tail :e e})
            (binding [ast/*verbose* true]
              (ast/inspect (ast/application f tail)))
            (println e)
            :error))))))

(defn primitive [n f]
  (ast/extern n (call-primitive-fn f)))

(defn primitives [m]
  (reduce (fn [acc [k v]] (assoc acc (ast/symbol k) (primitive k v))) {} m))

(defn nth* [c i]
  (nth c (dec i)))

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
    "nth*"   nth* ; base 1 indexing
    }))

;;;;; specialish forms

(defmacro check-tail [opts form body]
  {:style/indent 2}
  `(if (ast/incomplete? (:tail ~form))
     (let [next# (update ~form :tail i/walk (assoc ~opts :freeze? true))]
      (debug/trace! "delaying μ" ~form "->" next#)
       next#)
     ~body))

(defn μ [{args :tail :as app} opts]
  (let [args (if (ast/incomplete? args) (i/walk args opts) args)]
    (if (ast/incomplete? args)
      (assoc app :tail args)
      (if-let [args (env/μ-prepare args)]
        (do
          (debug/trace! "building μ" args)
          (i/walk (apply ast/μ args) opts))
        (let [next (update app :tail
                           (fn [t]
                             (conj (mapv #(i/walk % (assoc opts :freeze? true))
                                         (butlast t))
                                   (last t))))]
          (debug/trace! "postponing μ" app "->" next)
          next)))))

;; FIXME: `emit` doesn't need to be special. Why is it again?
(defn emit [{kvs :tail :as app} opts]
  (assert (even? (count kvs)))
  (ast/emission (mapv (fn [[k v]] [(ast/immediate k) v]) (partition 2 kvs))))

(defn with-channels [{[chmap body] :tail :as app} opts]
  (if (ast/incomplete? chmap)
    (update app :tail i/walk opts)
    (ast/ctx chmap body)))

(defn macros [m]
  (reduce (fn [acc [k f]]
            (assoc acc (ast/symbol k) (ast/extern k f))) {} m))

(def special
  "things that would traditionally be special forms."
  (macros
   {"μ"      μ
    "emit"   emit

    "with-channels" with-channels

    ;; TODO: builtin macros needed for a working system.
    ;;
    ;; emit-recur
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
  (reduce (fn [e [k v]] (env/ns-intern e k v)) env/empty-ns (merge special fns)))
