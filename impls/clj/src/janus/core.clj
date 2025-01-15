(ns janus.core
  (:refer-clojure :exclude [eval])
  (:require
   [janus.ast :as ast]
   [janus.builtins :refer [base-env]]
   [janus.i4 :as i]
   [janus.reader :as r]
   [janus.runtime :as rt]
   [taoensso.telemere :as t]))

(defn form-log! [level form msg]
  (t/log! {:level level
           :data  (assoc (select-keys (meta form) [:string :file :line :col])
                         :form form)}
                msg))

(def srcpath "../../src/")
(def corexprl (str srcpath "core.xprl"))
(def testxprl (str srcpath "test.xprl"))

(def env (atom base-env))

(def o (atom nil))

(defn eval [form conts]
  (rt/push! (rt/task #(i/eval % {} conts) form {:name "eval"})))

(defn loadfile [env fname]
  ;; HACK: The executor doesn't clean up properly in the face of errors!
  (rt/stop!)

  (let [conts {rt/env    #(swap! env merge %)
               rt/return #(throw (RuntimeException. "boom!"))
               rt/error  (fn [{:keys [form message]}]
                           (t/log! {:id   :fileloader :level :error
                                    :data form}
                                   message))}]
    ;; HACK: This repl will not work if we enable multiple executors and work
    ;; stealing. This is because it depends on the order in which the outputs of
    ;; `xprl-def` are executed.
    ;;
    ;; TODO: Rewrite this as soon as statefuls are implemented.
    (letfn [(looper [reader]
              (let [reader (r/read reader @env)
                    form   (:form reader)]
                (form-log! :debug form "eval form")
                (if (= :eof form)
                  @env
                  (eval form (i/with-return conts
                               (fn [res]
                                 (println res)
                                 (looper reader)))))))]
      (looper (r/file-reader fname)))))

(def ^:dynamic *t nil)

(defn tset! [v]
  (alter-var-root (var *t) (fn [& args] v)))

#_(defn ktest
  "Kludge testing. Takes a test file, reads and evaluates forms in pairs, and
  asserts the results to be equal. If not, halt testing and set *t to the lhs
  (after read but before eval)."
  [env fname]
  ;; Don't modify outside environment.
  (let [env (atom (if (instance? clojure.lang.IDeref env) @env env))
        c   {rt/env   #(swap! env merge %)
             rt/error #(t/event! :ktest {:data % :msg "bbom"})}]
    (letfn [(comparator [r1 r2]
              (let [[f1 f2] (map :form [r1 r2])]
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
                (tset! (:form init-form))))
            (looper [r]
              (let [r1      (r/read r @env)
                    f1      (:form r1)
                    r2      (r/read r1 @env)
                    f2      (:form r2)
                    collect (rt/collector (comparator r1 r2) 2)]
                (t/log! {:id   :ktest :level :debug
                         :data {:lhs r1 :rhs r2}}
                        "Running test:")
                (if (or (= :eof f1) (= :eof f2))
                  (t/log! {:level :info :id :ktest} "All tests passed!")
                  (rt/push!
                   rt/*executor*
                   [(rt/task #(apply i/eval %)
                             [f1 {} (rt/withcc c
                                      rt/return #(rt/receive collect 0 %)
                                      rt/error  (handler r1))])
                    (rt/task #(apply i/eval %)
                             [f2 {} (i/with-return c
                                      #(rt/receive collect 1 %))])]))))]
      (looper (r/file-reader fname)))))

(defn ev [s]
  (eval (:form (r/read (r/string-reader s) @env))
        {rt/return  #(reset! o %)
         rt/unbound (fn [x] (println "Unbound!" x))
         rt/error (fn [e] (t/error! e))})
  ;; @o
  )

(add-watch o ::top (fn [_ _ _ v] (println v)))

(defn re [env]
  (let [form (r/read (r/stdin-reader) @env)]
    (eval (:form form) {rt/return #(reset! o %)
                        rt/env    (fn [e']
                                    (println e')
                                    (swap! env merge e'))
                        rt/error  #(t/log! :error %)}))
  @o)

(defn repl [& args]
  (t/set-min-level! :warn)
  (let [env (atom base-env)]
    (println "Loading core.xprl")
    (loadfile env corexprl)
    (println "Ready.")
    (println)
    (print ">> ")
    (flush)
    (loop [f (re env)]
      (println f)
      (print ">> ")
      (flush)
      (when (not= f :eof)
        (recur (re env))))))

(defn clear-filters! []
  (t/set-min-level! :warn)
  (t/set-min-level! :janus.i4/trace :info)
  (t/set-min-level! :event #{"janus.runtime"} :info)
  (t/set-id-filter! "*")
  (t/set-ns-filter! "*"))

(defn ev-filters! []
  (t/set-min-level! :janus.i4/trace :trace))

(defn rt-filters! []
  (t/set-min-level! :event #{"janus.runtime"} :trace))

(t/set-min-level! :error :debug)
