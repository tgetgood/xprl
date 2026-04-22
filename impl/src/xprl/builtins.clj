(ns xprl.builtins
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.ns :as ns]
   [xprl.system :as sys]))

(defmacro extern [[stateform envform argsform] & more]
  `(fn [state# env# self# args#]
     (let [args# (if (vector? args#) args# (i/walk state# env# args#))]
       (if (vector? args#)
         (let [~argsform args#
               ~stateform state#
               ~envform env#
               ~argsform (if ~(= :ensure (first more))
                           (if ~(second more) args# (i/walk state# env# args#))
                           args#)]
           (if (or ~(not= :ensure (first more)) ~(second more))
             ~(last more)
             (if (ast/incomplete? ~argsform)
               (ast/application env# self# ~argsform)
               (throw (RuntimeException.
                       (str "Invalid args passed to " (:name self#)
                            ".\nExpected: "
                            ~(str (second more)) "\nReceived: "
                            ~(cond
                              (symbol? argsform) {(name argsform) `~argsform}
                              (vector? argsform)
                              (apply hash-map
                                     (interleave
                                      (map (comp ast/symbol name) argsform)
                                      `~argsform))
                              true (str argsform " : " `~argsform))))))))
         (ast/application env# self# args#)))))

(defmacro defextern [mac args & more]
  `(def ~mac (extern ~args ~@more)))

;;;;; simple primitive fns

(defn noop [state env self args]
  (ast/application env self args))

(defn call-primitive-fn
  "given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (extern [_ env tail]
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

    "list?*"   ast/list?
    "map?*"    ast/map?
    "dot?*"    ast/dot?
    "string?*" string?

    "merge*"  merge
    "get*"    get

    ;; REVIEW: All symbols in a form are converted to Refs on read. The only
    ;; time we get raw Symbols back is when a μ shadows a Ref with its
    ;; parameter. So when we say `symbol?` in xprl we could mean either. But
    ;; then should they be distinguishable in the language?
    "symbol?*" ast/symbolic?
    }))

;;;;; specialish forms

;; These are special because they can operate on a vector even if the elements
;; of that vector can't yet be computed. It would be correct to wait until they
;; were, but this prunes a lot of unecessary work and is (I think) worth the
;; complexity.
(defextern nth* [_ _ [x i]]
  :ensure (and (vector? x) (int? i))
  (nth x (dec i)))

(defextern first* [_ _ [x]]
  :ensure (ast/coll? x)
  (first x))

(defextern rest* [_ _ [x]]
  :ensure (ast/coll? x)
  (into [] (rest x)))

(defextern count* [_ _ [x]]
  :ensure (ast/coll? x)
  (count x))

(defextern empty?* [_ _ [x]]
  :ensure (ast/coll? x)
  (boolean (empty? x)))

(defextern emit [state env kvs]
  (do (assert (even? (count kvs)))
      (->> kvs
           (partition 2)
           (mapv (fn [[k v]] [(ast/immediate k) v]))
           (i/walk state env)
           (sys/try-emissions! state env))))

(defextern with-channels [state env [ctx body]]
  :ensure (ast/map? ctx)
  (i/walk state (env/merge-ctx env ctx) body))

(defextern μ [s e [param body]]
  :ensure (ast/symbolic? param)
  (let [id    (gensym "μ-param-")
        param (ast/symbol param)
        s' (-> s (assoc :μ? true) (env/capture param id))]
    (ast/μ e id param (i/walk s' e body))))

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
    "first*"        first*
    "rest*"         rest*
    "count*"        count*
    "empty?*"       empty?*
    ;; TODO: These need runtime impls.
    "pipe"          noop
    "net"           noop}))

;;;;; The Ur context from which all programs derive.
;;
;; I don't really like this being so ad hoc. There will have to be a takeover
;; moment when the intended long term context and history system is finally
;; built. that is to say there will be a shock in the history where we suddenly
;; have no past, no origin. why is bootstrapping so singular like that?

(def base-env
  (reduce (fn [e [k v]] (ns/ns-intern e k v)) ns/empty-ns (merge special fns)))
