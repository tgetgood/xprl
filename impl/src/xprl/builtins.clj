(ns xprl.builtins
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.ns :as ns]
   [xprl.system :as sys]))

(defmacro extern [[envform argsform] & more]
  `(fn [env# self# args#]
     (let [args# (if (vector? args#) args# (i/walk env# args#))]
       (if (vector? args#)
         (let [~argsform args#
               ~envform env#
               ~argsform (if ~(= :ensure (first more))
                           (if ~(second more) args# (i/walk env# args#))
                           args#)]
           (if (or ~(not= :ensure (first more)) ~(second more))
             ~(last more)
             (ast/application env# self# ~argsform)))
         (ast/application env# self# args#)))))

(defmacro defextern [mac [envform argsform] & more]
  `(def ~mac (extern [~envform ~argsform] ~@more)))

;;;;; simple primitive fns

(defn call-primitive-fn
  "given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (extern [env tail]
    :ensure (not (ast/incomplete? tail))
    (try
        (apply f tail)
        (catch Exception e
          (reset! debug/*pfn {:f f :args tail :e e})
          (binding [ast/*verbose* true]
            (ast/inspect (ast/application f tail)))
          (println e)
          :error))))

(defn primitive [n f]
  (ast/extern n (call-primitive-fn f)))

(defn primitives [m]
  (reduce (fn [acc [k v]] (assoc acc (ast/symbol k) (primitive k v))) {} m))

(defn rest* [xs]
  (into [] (rest xs))) ; no linked lists!

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

;;;;; specialish forms

;; nth* is special because it can operate on a vector even if the elements of
;; that vector can't yet be computed. It would be correct to wait until they
;; were, but this prunes a lot of unecessary work and is (I think) worth the
;; complexity.
(defextern nth* [_ [x i]]
  :ensure (and (vector? x) (int? i))
  (nth x (dec i)))

(defextern emit [env kvs]
  (do (assert (even? (count kvs)))
      (let [msgs (i/walk env (mapv (fn [[k v]] [(ast/immediate k) v]) (partition 2 kvs)))]
        (sys/try-emissions! env msgs))))

(defextern with-channels [env [ctx body]]
  :ensure (ast/map? ctx)
  (i/walk (env/merge-ctx env ctx) body))

(defextern μ [env [param body]]
  :ensure (ast/symbolic? param)
  (let [id    (gensym "μ-param-")
        env   (assoc env :μ? true)
        param (ast/symbol param)]
    (ast/μ env id param (i/walk env (env/capture body param id)))))

(defn macros [m]
  (reduce (fn [acc [k f]]
            (assoc acc (ast/symbol k) (ast/extern k f))) {} m))

(def special
  "things that would traditionally be special forms."
  (macros
   {"μ"             μ
    "emit"          emit
    "with-channels" with-channels
    "nth*"          nth*

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
  (reduce (fn [e [k v]] (ns/ns-intern e k v)) ns/empty-ns (merge special fns)))
