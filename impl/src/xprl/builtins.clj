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
  (fn [{tail :tail :as form} env]
    (if (or (ast/incomplete? tail) (some ast/incomplete? tail))
      (update form :tail i/walk env)
      (try
        (apply f tail)
        (catch Exception e
          (reset! debug/*pfn {:f f :args tail :env env :e e})
          (binding [ast/*verbose* true]
            (ast/inspect (ast/application f tail)))
          (println e)
          :error)))))

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

    "symbol?*" ast/symbol?

    "first*" first
    "rest*"  rest*

    "count*" count
    "nth*"   nth* ; base 1 indexing

    }))

;;;;; specialish forms

(defmacro check-tail [env form body]
  {:style/indent 2}
  `(if (ast/incomplete? (:tail ~form))
     (update ~form :tail i/walk (assoc ~env :μ? true))
     ~body))

(defn μ [{args :tail :as app} env]
  (check-tail env app
    (if-let [args (env/capture args env)]
      (apply ast/μ args)
      ;; if the names don't resolve, then there has to be a μ context
      ;; surrounding our current context.
      (update app :tail i/walk env))))

(defn emit [{kvs :tail :as app} _]
  (assert (even? (count kvs)))
  (ast/emission (mapv (fn [[k v]] [(ast/immediate k) v]) (partition 2 kvs))))

;; TODO: revisit the smalltalk style impl of branching. I think I can control
;; evaluation better that way and not have to worry about walking branches not
;; taken and all of the possible errors that come with that.
(defn select [{[p t f] :tail :as app} env]
  (let [env (assoc env :μ? true)]
    (check-tail env app
      ;; FIXME: What the hell do we do if there's an `emit` in the condition of a
      ;; select? Just kick them up along with emissions from the winning branch?
      ;; That's logical, but what a shitshow. And whose job is it to make sure
      ;; that happens properly

      ;; first walk *just p*. that's important.
      (let [p' (i/walk p env)]
        (if (ast/incomplete? p')
          ;; if p is not a bool, it ~should~ be safe to walk both `t` & `f`...
          (assoc app :tail [p' (i/walk t env) (i/walk f env)])
          ;; if p resolves, don't walk the dead branch: it might not be safe to do so.
          ;; e.g. (select ~(empty? xs) [] ~(first xs))
          (do
            (assert (boolean? p'))
            (if p' t f)))))))

(defn with-channels [{[chmap body] :tail :as app} env]
  (if (ast/incomplete? chmap)
    (update app :tail i/walk env)
    (ast/ctx chmap body)))

;; TODO: builtin macros needed for a working system.
;;
;; emit-recur
;; pipe
;; net


(defn macros [m]
  (reduce (fn [acc [k f]]
            (assoc acc (ast/symbol k) (ast/extern k f))) {} m))

(def special
  "things that would traditionally be special forms."
  (macros
   {"μ"      μ
    "select" select
    "emit"   emit

    "with-channels" with-channels

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
