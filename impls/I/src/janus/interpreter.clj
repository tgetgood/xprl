(ns janus.interpreter
  (:refer-clojure :exclude [eval apply reduce])
  (:require
   [clojure.string :as str]
   [janus.ast :as ast]
   [janus.runtime :as rt]
   [taoensso.telemere :as t]))

(defn unbound-error [s]
  (t/log! :error {:data (assoc (select-keys (meta s) [:string :file :line :col])
                               :symbol s)
                  :msg "Unbound symbol!"}))

(defn event! [type x dyn ccs]
  ;; FIXME: This should log different fields based on type and x.
  ;; Why is the type of x nil when x is a record from AST?
  (let [tt (type x)]
    (t/event! ::???
              {:level :trace
               :kind  ::trace
               :data  {:dyn  dyn
                       :type tt
                       :form x}})))

;;;;; Helpers to simplify CPS transform

(defn continue [ccs k msg]
  (rt/>! (get ccs k) msg))

(defn return {:style/indent 1} [ccs x]
  (continue ccs rt/return x))

(defn with-return
  "Creates a new channel, wires `next` onto it as its continuation, and returns
  a new cable with the new continuation set as the return wire."
  {:style/indent 1}
  [c next]
  (let [ret (rt/chan)]
    (rt/<! ret next) ; continue with next when we have a value
    (rt/withcc c rt/return ret))) ; divert return values to ret

;;;;; Indirection to separate telemetry from logic

(defprotocol Reduce
  (reduce* [x dyn ccs]))

(defn reduce {:style/indent 2} [f dyn ccs]
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

(defn reduce-coll [acc xs dyn ccs]
  (if (empty? xs)
    (return ccs acc)
    (reduce (first xs) dyn
      (with-return ccs
        (fn [x]
          (reduce-coll (conj acc x) (rest xs) dyn ccs))))))

;;;;; Interpreter core

(extend-protocol Reduce
  Object
  (reduce* [x _ ccs] (return ccs x))

  janus.ast.Immediate
  (reduce* [x dyn ccs]
    (eval (rt/wire (map :form) x) dyn ccs))

  janus.ast.Application
  (reduce* [{:keys [head tail]} dyn ccs]
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs))))

  clojure.lang.AMapEntry
  (reduce* [x dyn ccs]
    (reduce-coll [] [(key x) (val x)] dyn ccs))

  clojure.lang.APersistentVector
  (reduce* [xs dyn ccs]
    (reduce-coll [] xs dyn ccs))

  clojure.lang.APersistentMap
  (reduce* [m dyn ccs]
    (reduce-coll {} m dyn ccs)))

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

(defn createσ [_ args dyn ccs]
  (let [[init flush-fn next-fn] args]
    ))
