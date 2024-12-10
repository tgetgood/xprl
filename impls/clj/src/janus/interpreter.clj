(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce reduced?])
  (:require [clojure.pprint :as pp]
            [clojure.walk :as walk]
            [janus.ast :as ast]
            [janus.runtime :as rt]
            [janus.util :refer [fatal-error!]]
            [taoensso.telemere :as t]))

(defprotocol Reductive
  (reduced? [this])
  (reduce [this env c]))

(defprotocol Evaluable
  (eval [this env c]))

(defprotocol Applicable
   (apply [this args env c]))

;; FIXME: just find all calls to this
(defn ni [] (throw (RuntimeException. "not implemented")))

(defn with-return {:style/indent 1} [c next]
  (rt/withcc c rt/return next))

(defn event!
  [id m]
  (t/event! id {:level :trace :data m :kind ::trace}))

(defn succeed [c v]
  (rt/emit c rt/return v))

(def marker ::unbound)

;;;;; Mu

(defprotocol MuP
  (register-delay! [this sym return-continuation])
  (set-static! [this value] "For constant functions.")
  (activate! [this env calling-continuations]))

(defprotocol Inspectable
  (inspect [this]))

(defn mustr [params body]
  (str "(#μ " params " " body ")"))

(deftype Mu [name
             params
             body
             ^:volatile-mutable loose-ends
             ^:volatile-mutable cached-value]
  Object
  (toString [_]
    (str "(#μ " params " " body ")"))

  Inspectable
  (inspect [_]
    {:params params :body body :name name
     :loose-ends loose-ends :cached cached-value})

  MuP
  (set-static! [_ value]
    (set! cached-value value)
    (event! ::createμ.return.static value))
  (register-delay! [_ sym rc]
    (set! loose-ends (update loose-ends sym conj rc))
    (event! ::createμ.delay {:symbol sym :loose-ends loose-ends}))
  (activate! [_ env ccs]
    (event! ::createμ.activation {:env env})
    (if (not (nil? cached-value))
      (rt/emit ccs rt/return cached-value)
      (clojure.core/apply
       rt/emit ccs
       (mapcat (fn [[k vs]]
                 (map (fn [v] [eval [k env (assoc ccs rt/return v)]]) vs))
               loose-ends)))))

(defn μ [name params body]
  (->Mu name params body {} nil))

(defn validate-μ [args]
  (condp = (count args)
    3 args
    2 [nil (first args) (second args)]))

(defn createμ [_ tail dyn ccs]
  (let [[name params body] (validate-μ tail)
        the-μ              (μ name params body)]
    (letfn [(delay [[sym ccs]]
              (register-delay! the-μ sym (get ccs rt/return)))
            (ret [value]
              (set-static! the-μ value))
            (next [params']
              (let [bind (into {} (map (fn [k] [k marker]))
                               (ast/bindings params))
                    env' (merge dyn bind (when name {name marker}))]
                (event! ::createμ.params params')
                (reduce body env' (rt/withcc ccs rt/return ret rt/delay delay))))]
      (event! ::createμ {:args tail :dyn dyn})
      (rt/emit ccs
        (fn [args] (clojure.core/apply reduce args))
        [params dyn (rt/withcc ccs rt/return next rt/delay delay)]
        rt/return the-μ))))

(extend-protocol Reductive
  Object
  (reduced? [_] true)
  (reduce [o env c]
    (event! ::reduce.fallthrough {:form o :type (type o)})
    (succeed c o))

  clojure.lang.APersistentVector
  (reduced? [x] (every? reduced? x))
  (reduce [this env c]
    (if (::reduced? (meta this))
      ;; The current implementation re-reduces expressions quite often, so this
      ;; is just a cache.
      ;; TODO: Do this more systematically. Maybe as a layer.
      (do
        (event! ::reduce.vector.noop {:form this})
        (succeed c this))
      (do
        (event! ::reduce.vector {:form this :dyn env})
        (let [next (fn [v]
                     (succeed c (with-meta v
                                  (assoc (meta this) ::reduced? (reduced? v)))))
              collector (rt/collector (with-return c next) (count this))
              runner (with-meta
                       (fn [[i x]]
                         (event! :reduce.vector.runner {:i i :x x})
                         (reduce x env
                                 (with-return c
                                   (fn [v] (rt/receive collector i v)))))
                       {:name "reduce vector runner"})
              tasks (interleave (repeat runner) (map-indexed vector this))]
          (event! ::reduce.vector.tasks tasks)
          (clojure.core/apply rt/emit c tasks)))))

  clojure.lang.AMapEntry
  (reduced? [x] (and (reduced? (key x)) (reduced? (val x))))
  (reduce [this env c] (ni))

  clojure.lang.APersistentMap
  (reduced? [x] (every? reduced? x))
  (reduce [this env c] (ni))

  clojure.lang.APersistentSet
  (reduced? [x] (every? reduced? x))
  (reduce [this env c] (ni))

  janus.ast.Immediate
  (reduced? [_] false)
  (reduce [x dyn c]
    (let [env' (merge (:env x) dyn)]
      (event! ::reduce.Immediate {:form x :env (:env x) :dyn dyn :merged env'})
      (eval (:form x) env' c)))

  janus.ast.Application
  (reduced? [_] false)
  (reduce [{:keys [head tail env] :as x} dyn c]
    ;; An application carries bindings with it from the context of its
    ;; definition, but those can be overridden by bindings in the context in
    ;; which is finds itself embedded. This is basically classic dynamic scope.
    (let [env' (merge env dyn)]
      (event! ::reduce.Application {:head head :tail tail :env env :dyn dyn})
      (reduce head env' (with-return c #(apply % tail env' c)))))

  janus.ast.Pair
  (reduced? [x] (and (reduced? (:head x)) (reduced? (:tail x))))
  (reduce [x env c]
    (event! ::reduce.Pair {:form x :dyn env})
    (succeed c x)))

(extend-protocol Evaluable
  Object
  (eval [o env c]
    (event! :eval/fallthrough {:form o :type (type o)})
    (succeed c o))

  clojure.lang.APersistentVector
  (eval [this env c]
    (event! :eval/Vector {:form this :dyn env})
    (reduce (mapv #(ast/immediate % env) this) env c))

  janus.ast.Symbol
  (eval [this env c]
    ;; (event! ::eval.symbol {:form this :dyn env :lex (:env (meta this))})
    (if-let [v (get env this)]
      (if (= v ::unbound)
        (do
          (event! ::eval.symbol.unbound (select-keys env [this]))
          (rt/emit c rt/delay [this c])
          #_(succeed c (ast/immediate this env)))
        (do
          (event! ::eval.symbol.dynamic (select-keys env [this]))
          ;; REVIEW: The value of every dynamic binding should be a context
          ;; switch, so the old dyn env is irrelevant after lookup.
          (reduce v {} c)))
      ;; What has two backs and carries its meaning on each?
      (if-let [v (-> this meta :lex (get this))]
        (do
          (event! ::eval.symbol.lexical (-> this meta :lex (select-keys [this])))
          ;; REVIEW: When we step into a lexically bound form, then our dynamic
          ;; env is no longer relevant. That's because "lexical env" in this
          ;; context means things that were defined in the namespace in which
          ;; the form currently being evaluated is defined. That is: lexically
          ;; bound values must have been fully defined at a point strictly prior
          ;; to the point at which evaluation of the current form commenced.
          ;; Thus nothing in our current dynamic env can possibly apply to
          ;; anything in our lexical env.
          ;;
          ;; TODO: Clean that up and put it in the documentation.
          (reduce v {} c))
        (fatal-error! c this "unbound symbol"))))

  janus.ast.Pair
  (eval [{:keys [head tail] :as this} env c]
    (event! ::eval.Pair {:form this :dyn env})
    (reduce (ast/application (ast/immediate head env) tail env) env c))

  janus.ast.Immediate
  (eval [{:keys [form env] :as this} dyn c]
    (let [env' (merge env dyn)]
      (event! ::eval.Immediate {:form form :env env :dyn dyn})
      (letfn [(next [form]
                (if (instance? janus.ast.Immediate form)
                  (throw (RuntimeException. "unreachable??"))
                  #_(succeed c (ast/immediate this env'))
                  (eval form env' c)))]
        (eval form env' (with-return c next)))))

  janus.ast.Application
  (eval [form dyn c]
    (let [env' (merge (:env form) dyn)]
      (event! ::eval.Application {:form form :env env'})
      (letfn [(next [form]
                (if (instance? janus.ast.Application form)
                  (throw (RuntimeException. "unreachable??"))
                  #_(succeed c (ast/immediate form env'))
                  (eval form env' c)))]
        (reduce form env' (with-return c next))))))

(extend-protocol Applicable
  janus.ast.Immediate
  ;; Immediates cannot be applied; we must backtrack and wait until the
  ;; immediate can be evaled.
  (apply [head tail env c]
    (event! ::apply.Immediate {:form [head tail]})
    (succeed c (ast/application head tail env)))

  janus.ast.Application
  (apply [head tail env c]
    (event! ::apply.Application {:data [head tail]})
    (letfn [(next [head']
              (if (instance? janus.ast.Application head)
                (succeed c (ast/application head' tail env))
                (apply head' tail env c)))]
      (reduce head env (with-return c next))))

  Mu
  (apply [head tail env c]
    (event! ::apply.Mu {:head head :tail tail :dyn env})
    (letfn [(next [tail']
              (let [meta (inspect head)
                    bind (merge (ast/destructure (:params meta) tail')
                                ;; If a μ is named, then bind its name to it
                                ;; in the env of the body when applying
                                ;; arguments. This prevents circular links when
                                ;; creating the μ in the first place.
                                (when (:name meta)
                                  {(:name meta) head}))]
                (event! ::apply.Mu.destructuring {:bindings bind})
                (activate! head bind c)))]
      (reduce tail env (with-return c next))))

  janus.ast.PrimitiveMacro
  (apply [head tail env c]
    (event! ::apply.Macro [head tail (dissoc (meta tail) :lex)])
    (letfn [(next [v] (succeed c v))]
      ((:f head) head tail env (with-return c next))))

  janus.ast.PrimitiveFunction
  (apply [head tail env c]
    (event! ::apply.Fn {:form [head tail] :dyn env})
    (letfn [(next [tail']
              (event! ::apply.Fn.tail {:form tail'})
              (succeed c (if (reduced? tail')
                           (clojure.core/apply (:f head) tail')
                           (ast/application head tail' env))))]
      (reduce tail env (with-return c next)))))
