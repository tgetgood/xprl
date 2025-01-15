(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce])
  (:require
   [clojure.string :as str]
   [janus.ast :as ast]
   [janus.emission :as emit]
   [janus.runtime :as rt]
   [taoensso.telemere :as t]))

(defn unbound-error [s]
  (t/log! :error {:data (assoc (select-keys (meta s) [:string :file :line :col])
                               :symbol s)
                  :msg "Unbound symbol!"}))

(defn event! [type x dyn ccs]
  ;; FIXME: This should log different fields based on type and x.
  ;; Why is the type of x nil when x is a record from AST?
  #_(t/event! (keyword (.name *ns*) (str/join "." [(name type) (.getName (type x))]))
            {:level :trace
             :kind  ::trace
             :dyn   dyn
             :form  x}))

;;;;; Helpers to simplify CPS transform

(defn continue [ccs [k msg]]
  (push! (task (get ccs k) msg)))

(defn return {:style/indent 1} [ccs x]
  (continue ccs [rt/return x]))

(defn with-return {:style/indent 1} [c next]
  (emit/withcc c rt/return next))

;;;;; Indirection to separate telemetry from logic

(defprotocol Reduce
  (reduce* [x dyn ccs]))

(defn reduce{:style/indent 2} [f dyn ccs]
  (event! :reduce f dyn ccs)
  (reduce* f dyn ccs))

(defprotocol Eval
  (eval* [x dyn ccs]))

(defn eval {:style/indent 2} [x dyn ccs]
  (event! :eval x dyn ccs)
  (eval* x dyn ccs))

(defprotocol Apply
  (apply* [head tail dyn ccs]))

(defn apply {:style/indent 3} [head tail dyn ccs]
  (event! :apply [head tail] dyn ccs)
  (apply* head tail dyn ccs))

;;;;; Collections

(defn map-entry
  "Not a great name. Applies `f` to key and val of `e` and returns a new
  clojure.lang.MapEntry."
  [f e]
  (clojure.lang.MapEntry. (f (key e)) (f (val e))))

(defn reduce-collect! [coll xs dyn ccs]
  (rt/push!
   (map-indexed
    (fn [i arg]
      (rt/task (fn [e] (reduce e dyn (with-return ccs #(coll i %)))) arg))
    xs)))

;;;;; Interpreter core

(extend-protocol Reduce
  Object
  (reduce* [x _ ccs] (return ccs x))

  janus.ast.Immediate
  (reduce* [{:keys [form]} dyn ccs]
    (eval form dyn ccs))

  janus.ast.Application
  (reduce* [{:keys [head tail]} dyn ccs]
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs))))

  clojure.lang.AMapEntry
  (reduce* [x dyn ccs]
    (reduce-collect!
     (rt/ordered-collector 2
       (fn [[k v]] (return ccs
                     (with-meta (clojure.lang.MapEntry. k v) (meta x)))))
     [(key x) (val x)] dyn ccs))

  clojure.lang.APersistentVector
  (reduce* [xs dyn ccs]
    (reduce-collect!
     (rt/ordered-collector (count xs) #(return ccs (with-meta % (meta xs))))
     xs dyn ccs))

  clojure.lang.APersistentMap
  (reduce* [m dyn ccs]
    (let [c (rt/unordered-collector {} #(return ccs (with-meta % (meta m))))]
      (rt/push!
       (map #(rt/task (fn [e] (reduce e dyn (with-return ccs c))) %) m)))))

(extend-protocol Eval
  Object
  (eval* [x dyn ccs] (return ccs x))

  janus.ast.Symbol
  (eval* [s dyn ccs]
    (if-let [v (get dyn s)] ; s is a μ parameter
      (reduce v {} ccs)
      (if-let [v (-> s meta :lex (get s))] ; s is lexical from def
        (reduce v {} ccs)
        (unbound-error s))))

  janus.ast.Pair
  (eval* [{:keys [head tail]} dyn ccs] ; (I (P x y)) => (A (I x) y)
    (eval head dyn (with-return ccs #(apply % tail dyn ccs))))

  janus.ast.Immediate
  (eval* [{:keys [form]} dyn ccs]
    (eval form dyn (with-return ccs #(eval % dyn ccs))))

  janus.ast.Application
  (eval* [x dyn ccs] ; (I (A x y)) must be treated in applicative order.
    (reduce x dyn (with-return ccs #(eval % dyn ccs))))

  clojure.lang.APersistentVector ; (I (L x y ...)) => (L (I x) (I y) ...)
  (eval* [xs dyn ccs]
    (reduce (into [] (map ast/immediate) xs) dyn ccs))

  clojure.lang.AMapEntry ; (I {k v}) => {(I k) (I v)}
  (eval* [x dyn ccs]
    (reduce (map-entry ast/immediate x) dyn ccs))

  clojure.lang.APersistentMap ; L can represent any iterable collection in fact
  (eval* [xs dyn ccs]
    (reduce (into {} (map ast/immediate) xs) dyn ccs)))

(extend-protocol Apply
  janus.ast.Application       ; (A (A x y) z) proceeds from inside out:
  (apply* [head tail dyn ccs] ; "Applicative on the left".
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs))))

  janus.ast.PrimitiveMacro   ; a pMac must invoke ccs or the computation will
  (apply* [mac tail dyn ccs] ; wither away.
    ((:f mac) mac tail dyn ccs)) ; We pass the macro itself in for its metadata.

  janus.ast.PrimitiveFunction
  (apply* [f args dyn ccs]
    (reduce args dyn
      (with-return ccs #(return ccs (clojure.core/apply (:f f) %)))))

  janus.ast.Mu
  (apply* [μ args dyn ccs]
    (reduce args dyn
      (with-return ccs
        (fn [args]
          (let [bind (ast/destructure (:params μ) args)
                Δenv (if (= ::anon (:name μ)) bind (assoc bind (:name μ) μ))]
            (reduce (:body μ) (merge dyn (:dyn μ) Δenv) ccs)))))))

(defn createμ [_ args dyn ccs]
  (cond
    ;; TODO: docstrings and metadata?
    (= 2 (count args)) (createμ _ [::anon (first args) (second args)] dyn ccs)
    (= 3 (count args)) (let [[name params body] args]
                         (reduce name dyn
                           (with-return ccs
                             (fn [name]
                               (reduce params dyn
                                 (with-return ccs
                                   #(return ccs
                                      (with-meta (ast/μ name % body dyn)
                                        (meta args)))))))))))
