(ns xprl.builtins
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]))

(def benv
  "Env for builtins to call back into the interpreter."
  ;; FIXME: the key :μ? is far too specific. Something like "supress-emission"
  ;; would be more to the point.
  {:μ? true})

;;;;; simple primitive fns

(defn call-primitive-fn
  "given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (fn [{tail :tail :as form}]
    (if (or (ast/incomplete? tail) (some ast/incomplete? tail))
      (update form :tail i/walk benv)
      (try
        (apply f tail)
        (catch Exception e
          (reset! debug/*pfn {:f f :args tail :e e})
          (ast/inspect (ast/application f tail))
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

(defmacro check-tail [form body]
  {:style/indent 1}
  `(if (ast/incomplete? (:tail ~form))
     (update ~form :tail i/walk benv)
     ~body))

(defn μ-ready? [args]
  (every? ast/symbol? (butlast args)))

(defn μ [{args :tail :as app}]
  (check-tail app
    (let [names (ast/list (butlast args))]
      (if (every? ast/symbol? names)
        (apply ast/μ (env/capture args))
        ;; if the names don't resolve, it ~should~ be safe to walk the body
        ;; review: but what if one of them resolves and the other doesn't?
        (update app :tail i/walk benv)))))

(defn emit [{kvs :tail :as app}]
  (assert (even? (count kvs)))
  (ast/emission
   (mapv (fn [[k v]] [(ast/immediate k) v]) (partition 2 kvs))))

(defn select [{[p t f] :tail :as app}]
  (check-tail app
    ;; FIXME: What the hell do we do if there's an `emit` in the condition of a
    ;; select? Just kick them up along with emissions from the winning branch?
    ;; That's logical, but what a shitshow. And whose job is it to make sure
    ;; that happens properly

    ;; first walk *just p*. that's important.
    (let [p' (i/walk p benv)]
      (if (ast/incomplete? p')
        ;; if p is not a bool, it ~should~ be safe to walk both `t` & `f`...
        (assoc app :tail [p' (i/walk t benv) (i/walk f benv)])
        ;; if p resolves, don't walk the dead branch: it might not be safe to do so.
        ;; e.g. (select ~(empty? xs) [] ~(first xs))
        (do
          (assert (boolean? p'))
          (if p' t f))))))

(defn with-channels [{[chmap body] :tail :as app}]
  #_(wait-until-evaluated
   [chmap]
   (i/walk (ast/ctx chmap body))))

;; todo: builtin macros needed for a working system.
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

    ;; "seq*"  (when-arg ast/seq)
    ;; "conc*" (when-arg ast/conc)

    ;; "first*" first*
    ;; "rest*"  rest*
    }))

;;;;; the ur context from which all programs derive.
;;
;; i don't really like this being so ad hoc. there will have to be a takeover
;; moment when the intended long term context and history system is finally
;; built. that is to say there will be a shock in the history where we suddenly
;; have no past, no origin. why is bootstrapping so singular like that?

(def base-env
  (reduce (fn [e [k v]] (env/ns-intern e k v)) env/empty-ns (merge special fns)))
