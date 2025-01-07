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

(defrecord Marker [id])

(defn marker [] (->Marker (gensym)))

;;;;; Mu

(defn activate-μ [μ env ccs]
    (event! ::createμ.activation {:env env :ends (count (:triggers μ))})
    (let [msgs (map (fn [[k ics]]
                      [#(clojure.core/apply eval %)
                       [k env (rt/withcc ccs rt/return (get ics rt/return))]])
                    (:triggers μ))]
      (event! ::createμ.activation.send msgs)
      (clojure.core/apply rt/emit ccs msgs)) )


(defn validate-μ [args]
  (condp = (count args)
    3 args
    2 [nil (first args) (second args)]))

(defn with-capture [ccs trap]
  (into {} (map (fn [[k _]]
                  [k (fn [msg]
                       (if (contains? trap k)
                         (let [f @(get trap k)]
                           (if (fn? f)
                             (f msg)
                             (throw
                              (RuntimeException. "trying to emit before activation."))))
                         (throw (RuntimeException. "unreachable??"))))]))
        ccs))

(defn createμ [_ tail dyn ccs]
  (let [[name params body] (validate-μ tail)
        mark               (marker)]
    (letfn [(next [params']
              (let [bind  (into {} (map (fn [k] [k mark]))
                                (ast/bindings params))
                    env'  (merge dyn bind (when name {name mark}))
                    trap  (into {} (map (fn [[k v]] [k (atom nil)])) ccs)
                    ;; TODO: Each mu needs a unique delay channel and unbound
                    ;; markers need to be unique to each μ, otherwise when
                    ;; nested, the inner μ will capture the symbols of the outer
                    ;; one.
                    delay (rt/unbound-collector
                           (fn [delays]
                             (rt/emit ccs
                               rt/return (ast/μ name params' body trap delays))))]
                (event! ::createμ.params params')
                (reduce body env' (rt/withcc (with-capture ccs trap)
                                    rt/delay delay))))])))


#_(defn createμ [_ tail dyn ccs]
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
      (clojure.core/apply rt/emit c tasks)))

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
      (if (instance? Marker v)
        (do
          (event! ::eval.symbol.unbound (select-keys env [this]))
          (rt/emit c rt/delay [this c]))
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

  janus.ast.Mu
  (apply [head tail env c]
    (event! ::apply.Mu {:head head :tail tail :dyn env})
    (letfn [(next [tail']
              (let [meta (meta head)
                    bind (merge env
                                (ast/destructure (:params meta) tail')
                                ;; If a μ is named, then bind its name to it
                                ;; in the env of the body when applying
                                ;; arguments. This prevents circular links when
                                ;; creating the μ in the first place.
                                (when (:name meta)
                                  {(:name meta) head}))]
                (event! ::apply.Mu.destructuring {:bindings bind})
                (activate-μ head bind c)))]
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
