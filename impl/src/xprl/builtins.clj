(ns xprl.builtins
  (:require
   [xprl.ast :as ast]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.ns :as ns]))

(defmacro extern [args & kws]
  (let [kws (apply hash-map kws)]
    (assert (and (contains? kws :ensure) (contains? kws :return))
            "structure forms need both :ensure and :return expressions.")
    `(let [ensure# (fn [& x#] (when (vector? (second x#)) (let [~args x#] ~(:ensure kws))))
           return# (fn [~@args] ~(:return kws))
           error#  (fn [head# tail#]
                     (throw (RuntimeException.
                             ;; TODO: pass on line and col info from reader.
                             ;; As is it's long lost by this point...
                             (str "Type mismatch in " (:name head#)
                                  "\nExpected: " ~(str (:ensure kws))
                                  "\nReceived: " '~args " = " tail#))))]
       (fn [env# self# args#]
         (try
           (let [args# (if (vector? args#) args# (i/walk env# args#))]
             (if (vector? args#)
               (let [args# (if (ensure# env# args#)
                             args# (i/walk env# args#))]
                 (if (ensure# env# args#)
                   (return# env# args#)
                   (if (ast/incomplete? args#)
                     (ast/application self# args#)
                     (error# self# args#))))
               (ast/application self# args#)))
           (catch Throwable e#
             (debug/trace!
              (with-out-str
                (binding [ast/*verbose* true]
                  (ast/inspect (ast/application self# args#)))))
             (let [msg# (str e# ":\n" (.getMessage e#) "\n" self# " " args#)]
               (ast/emission env# {(ast/xkey :error) [msg#]}))))))))


(defmacro defextern [mac args & more]
  `(def ~mac (extern ~args ~@more)))

;;;;; simple primitive fns

(defn call-primitive-fn
  "given an external (clojure) function, returns an applicative wrapper to call
  it from xprl."
  [f]
  (extern [_ tail]
    :ensure (not (ast/incomplete? tail))
    :return (apply f tail)))

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
    "symbol?*" ast/symbolic?}))

;;;;; specialish forms

;; These are special because they can operate on a vector even if the elements
;; of that vector can't yet be computed. It would be correct to wait until they
;; were, but this prunes a lot of unecessary work and is (I think) worth the
;; complexity.
(defextern nth* [_ [x i]]
  :ensure (and (vector? x) (int? i))
  :return (nth x (dec i)))

(defextern first* [_ [x]]
  :ensure (ast/coll? x)
  :return (first x))

(defextern rest* [_ [x]]
  :ensure (ast/coll? x)
  :return (into [] (rest x)))

(defextern count* [_ [x]]
  :ensure (ast/coll? x)
  :return (count x))

(defextern empty?* [_ [x]]
  :ensure (ast/coll? x)
  :return (boolean (empty? x)))

(defextern μ [env args]
  :ensure (and (ast/symbolic? (first args))
               (if (= 3 (count args)) (ast/symbolic? (second args)) true))
  :return (let [[name param body] (if (= 3 (count args)) args (into [nil] args))
                id                (gensym "μ-param-")
                recid             (gensym "μ-recur-")
                param             (ast/symbol param)
                env               (env/capture env param id)
                env               (if (nil? name) env (env/capture env name recid))]
    (ast/μ id recid name param (i/walk env body))))

(defextern emit [env kvs]
  :ensure (every? ast/keyword? (map first kvs))
  :return (let [cable (:cable env)]
            (assert (every? #(contains? cable (first %)) kvs) "undeliverable message!")
            ;; TODO: unbound and error channels.
            ;; All error messages should be delivered within the language.
            (ast/emission
             env
             (map (fn [[k v]] [(with-meta k {:wire (get cable k)}) v]) kvs))))

(defn macros [m]
  (reduce (fn [acc [k f]]
            (assoc acc (ast/symbol k) (ast/extern k f))) {} m))

;; noops always walk their tail because the effect of a noop is network based,
;; not semantic.
(defn noop [env self args]
  (ast/application self (i/walk env args)))

(def special
  "things that would traditionally be special forms."
  (macros
   {"μ"             μ
    "nth*"          nth*
    "first*"        first*
    "rest*"         rest*
    "count*"        count*
    "empty?*"       empty?*
    "emit"          emit
    "wire"          noop
    "with-channels" noop
    "net"           noop}))

;;;;; The Ur context from which all programs derive.
;;
;; I don't really like this being so ad hoc. There will have to be a takeover
;; moment when the intended long term context and history system is finally
;; built. that is to say there will be a shock in the history where we suddenly
;; have no past, no origin. why is bootstrapping so singular like that?

(def base-env
  (reduce (fn [e [k v]] (ns/ns-intern e k v)) ns/empty-ns (merge special fns)))
