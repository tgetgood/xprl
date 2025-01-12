(ns janus.i4
  (:refer-clojure :exclude [eval apply reduce])
  (:require
   [clojure.string :as str]
   [janus.ast :as ast]
   [janus.runtime :as rt]
   [taoensso.telemere :as t]))

(defn unbound-error [s]
  (t/log! :error {:data (assoc (select-keys (meta s) [:string :file :line :col])
                               :symbol s)
                  :msg "Unbound symbol! Stopping."}))

(defn event! [type x dyn ccs]
  ;; FIXME: This should log different fields based on type and x.
  (t/event! (keyword (.name *ns*) (str/join "." [(name type) (.getName (type x))]))
            {:level :trace
             :kind  ::trace
             :dyn   dyn
             :form  x}))

;;;;; Helpers to simplify CPS transform

(defn return {:style/indent 1} [ccs x]
  (rt/emit ccs rt/return x))

(defn with-return {:style/indent 1} [c next]
  (rt/withcc c rt/return next))

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

;;;;; Interpreter core

(extend-protocol Reduce
  Object
  (reduce* [x _ ccs] (return ccs x))

  janus.ast.Immediate
  (reduce* [{:keys [form]} dyn ccs]
    (eval form dyn ccs))

  janus.ast.Application
  (reduce* [{:keys [head tail]} dyn ccs]
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs)))))

(extend-protocol Eval
  Object
  (eval* [x dyn ccs] (return ccs x))

  janus.ast.Symbol
  (eval* [s dyn ccs]
    (if-let [v (get dyn s)] ; s is a μ parameter
      (reduce v {} ccs)
      (if-let [v (-> s meta :lex (get s))] ; s is lexical
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
    (reduce x dyn (with-return ccs #(eval % dyn ccs)))))

(extend-protocol Apply
  janus.ast.Application       ; (A (A x y) z) proceeds from inside out:
  (apply* [head tail dyn ccs] ; "Applicative on the left".
    (reduce head dyn (with-return ccs #(apply % tail dyn ccs))))

  janus.ast.PrimitiveMacro ; pMac must invoke ccs or the computation will dissolve.
  (apply* [mac tail dyn ccs]
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
            (reduce (:body μ) (merge dyn Δenv) ccs)))))))

(defn createμ [_ args dyn ccs]
  (cond
    ;; TODO: docstrings and metadata?
    (= 2 (count args)) (createμ [::anon (first args) (second args)])
    (= 3 (count args)) (let [[name params body] args]
                         (reduce params dyn
                           (with-return ccs
                             #(return ccs
                                (ast/μ name (reduce params) body)))))))
