(ns janus.core
  (:refer-clojure :exclude [test])
  (:require
   [janus.ast :as ast]
   [janus.builtins :as builtins]
   [janus.debug :as debug]
   [janus.env :as env]
   [janus.interpreter :as i]
   [janus.reader :as r]
   [janus.runtime :as rt]))

(def the-env (atom builtins/base-env))

;;;;; UI

(def srcpath "../src/")
(def recxprl (str srcpath "recur.xprl"))
(def core (str srcpath "core.xprl"))
(def td (str srcpath "base-transduction.xprl"))
(def testxprl (str srcpath "test.xprl"))

(defn env-channel-kludge [msg]
  (if (env/ctx? msg)
    (let [r (env-channel-kludge (:form msg))]
      [(first r) (assoc msg :form (second r))])
    msg))

(defn go!
  ([env f]
   (i/walk* env (debug/with-provenance (ast/immediate f)
                 {:origin ::repl :predecessor f})))
  ([env f ccs]
   (rt/schedule (ast/list [(fn [_] (rt/connect (go! env f) ccs))]))
   (rt/run!)))

(defn evv [s]
  (go! @the-env (:form (r/read (r/string-reader s)))))

(defn ev [s]
  (let [conts {(ast/xkeys :env)    (fn [l]
                                     (let [[sym value] (env-channel-kludge l)]
                                       (swap! the-env env/bind* sym value)))
               (ast/xkeys :return) println
               (ast/xkeys :error)  (fn [x]
                                     (println "Error: " x))}]
    (go! @the-env (:form (r/read (r/string-reader s))) conts)))


(defn iev [s]
  (ast/inspect (go! @the-env (:form (r/read (r/string-reader s))))))

(defn loadfile [envatom fname]
  (let [conts {(ast/xkeys :env)    (fn [l]
                                     (let [[sym value] (env-channel-kludge l)]
                                       (swap! envatom env/bind* sym value)))
               (ast/xkeys :return) #(throw
                                     (RuntimeException. "return to top level!"))
               (ast/xkeys :error)  (fn [x]
                                     (println "Error: " x))}]
    (loop [reader (r/file-reader fname)]
      (let [reader (r/read reader)
            form   (:form reader)]
        (if (= :eof form)
          'EOF
          (do
            (go! @envatom form (rt/with-return conts println))
            (recur reader)))))))

(defn reload! [fname]
  (reset! the-env builtins/base-env)
  (loadfile the-env fname))

(defmacro gs [n]
  `(-> @the-env (get-in [:names (ast/symbol ~(clojure.core/name n))])))

(defmacro inspect [n]
  `(ast/inspect (gs ~n)))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @the-env))))

(defn test []
  (let [conts {(ast/xkeys :env) (fn [l]
                                  (let [[sym value] l]
                                    (swap! the-env env/bind* sym value)))}]
    (loop [reader (r/file-reader testxprl)]
      (let [reader (r/read reader)
            form1  (:form reader)
            reader (r/read reader)
            form2  (:form reader)]
        (if (= :eof form1)
          'EOF
          (do
            (println "Evaluating: " form1)
            (println "---")
            (print "result: ")
            (go! @the-env form1 (rt/with-return conts println))
            (print "expected: " )
            (go! @the-env form2 (rt/with-return conts println))
            (recur reader)))))))

(def p debug/provenance)

(defn pp [x]
  (:predecessor (p x)))


(defmacro db [x]
  `(binding [debug/*verbose* true] ~x))
