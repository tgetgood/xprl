(ns janus.core
  (:require
   [janus.ast :as ast]
   [janus.builtins :refer [base-env]]
   [janus.interpreter :as i]
   [janus.reader :as r]
   [janus.runtime :as rt]
   [janus.util :as util]
   [taoensso.telemere :as t]))

(def srcpath "../../src/")
(def corexprl (str srcpath "core.xprl"))
(def testxprl (str srcpath "test.xprl"))

(def env (atom base-env))

(def cx (r/file-reader corexprl))

(def s
  (r/string-reader "[0x4e [{:asd 34} [#{:sd 34}]] \n;comment\n #_(f x y [23]) ~(bob x [1 2 3])]"))

(def forms (r/read-file corexprl) )

(def t (r/read (assoc (r/file-reader corexprl) :env @env)))


(def o (atom nil))

(defn loadfile [env fname]
  (let [conts {rt/env    #(reset! env %)
               rt/return #(throw (RuntimeException. "boom!"))}]
    ;; HACK: This repl will not work if we enable multiple executors and work
    ;; stealing. This is because it depends on the order in which the outputs of
    ;; `xprl-def` are executed.
    ;;
    ;; TODO: Rewrite this as soon as statefuls are implemented.
    (letfn [(looper [reader]
              (let [reader (r/read (assoc reader :env @env))
                    result (:result reader)]
                (util/form-log! :debug result "eval form")
                (if (= :eof result)
                  @env
                  (rt/pushngo!
                   [i/eval result {} (rt/withcc conts
                                       rt/return (fn [res]
                                                 (println res)
                                                 (looper reader)))]))))]
      (looper (r/file-reader fname)))))

(def ^:dynamic *t nil)

(defn tset! [v]
  (alter-var-root (var *t) (fn [& args] v)))

(defn ktest
  "Kludge testing. Takes a test file, reads and evaluates forms in pairs, and
  asserts the results to be equal. If not, halt testing and set *t to the lhs
  (after read but before eval)."
  [env fname]
  ;; Don't modify outside environment.
  (let [env (atom (if (instance? clojure.lang.IDeref env) @env env))
        c   {rt/env #(reset! env %) rt/error #(t/event! :ktest {:data % :msg
                                                                "bbom" })}]
    (letfn [(comparator [r1 r2]
              (let [[f1 f2] (map :result [r1 r2])]
                {rt/return
                 (fn [[v1 v2]]
                   (if (= v1 v2)
                     (do
                       (t/log! {:id   :ktest :level :info
                                :data {:expected [f1 f2]}}
                               "Success!")
                       (looper r2))
                     (do
                       (t/log! {:id   :ktest :level :error
                                :data {:expected [f1 f2]
                                       :results  [v1 v2]}}
                               "Failure!")
                       (tset!
                        (with-meta f1
                          (merge (meta f1)
                                 {:value v1 :expected v2}))))))}))
            (handler [init-form]
              (fn [{:keys [form message]}]
                (t/log! {:id   :ktest :level :error
                         :data {:form form
                                :meta (r/meta init-form)}}
                        message)
                (tset! (:result init-form))))
            (looper [r]
              (let [r1      (r/read (assoc r :env @env))
                    f1      (:result r1)
                    r2      (r/read (assoc r1 :env @env))
                    f2      (:result r2)
                    collect (rt/collector (comparator r1 r2) 2)]
                (t/log! {:id   :ktest :level :debug
                         :data {:lhs r1 :rhs r2}}
                        "Running test:")
                (if (or (= :eof f1) (= :eof f2))
                  (t/log! {:level :info :id :ktest} "All tests passed!")
                  (rt/pushngo!
                   [i/eval f1 {} (rt/withcc c
                                    rt/return #(rt/receive collect 0 %)
                                    rt/error  (handler r1))]
                   [i/eval f2 {} (rt/withcc c
                                    rt/return #(rt/receive collect 1 %))]))))]
      (looper (r/file-reader fname)))))

(defn r [form]
  (rt/pushngo! [i/eval form {} {rt/return #(reset! o %)}]))

(t/set-min-level! :info)
(t/set-id-filter! #{:collector/* :ktest :apply/* :eval/* :reduce/*} )
(t/set-ns-filter! "*")
