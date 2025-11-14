(ns xprl.core
  (:refer-clojure :exclude [test])
  (:require
   [xprl.ast :as ast]
   [xprl.builtins :as builtins]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.compiler :as c]
   [xprl.reader :as r]))

(def the-env (atom builtins/base-env))

;;;;; UI

(def srcpath "../src/")
(def recxprl (str srcpath "recur.xprl"))
(def core (str srcpath "core.xprl"))
(def td (str srcpath "base-transduction.xprl"))
(def testxprl (str srcpath "test.xprl"))

(def te (atom nil))

(defn env-updater [env]
  (fn [l]
    (reset! te l)
    (let [[sym value] l]
      (swap! env env/ns-intern sym value))))

(defn with-return [ccs cb]
  (assoc ccs (ast/xkeys :return) cb))

(defn go!
  ([env f] (i/interpret (ast/immediate (env/set-ns env f))))
  ([env f conts] (i/interpret (ast/ctx conts (ast/immediate (env/set-ns env f))))))

(defn evv [s]
  (go! @the-env (:form (r/read (r/string-reader s)))))

(defn ev [s]
  (let [conts {(ast/xkeys :env)    (env-updater the-env)
               (ast/xkeys :return) #(println (i/interpret %))
               (ast/xkeys :error)  (fn [x]
                                     (println "Error: " x))}]
    (go! @the-env (:form (r/read (r/string-reader s))) conts)))


(defn iev [s]
  (ast/inspect (go! @the-env (:form (r/read (r/string-reader s))))))

(defn loadfile [envatom fname]
  (let [conts {(ast/xkeys :env)    (env-updater envatom)
               (ast/xkeys :return) #(throw
                                     (RuntimeException. "return to top level!"))
               (ast/xkeys :error)  (fn [x]
                                     (println "Error: " x))}]
    (loop [reader (r/file-reader fname)]
      (let [reader (r/read reader)
            env    @envatom
            form   (:form reader)]
        (if (= :eof form)
          'EOF
          (do
            (go! @envatom form (with-return conts #(println (i/interpret %))))
            (recur reader)))))))

(defn reload! [fname]
  (reset! the-env builtins/base-env)
  (loadfile the-env fname))

(defmacro gs [n]
  `(env/lookup @the-env (ast/symbol ~(clojure.core/name n))))

(defmacro inspect [n]
  `(ast/inspect (gs ~n)))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @the-env))))

(defn test []
  (let [conts {(ast/xkeys :env)    (env-updater the-env)
               (ast/xkeys :return) #(println (i/interpret %))}
        retwrap (fn [f] (ast/pair (ast/symbol "emit")
                                  [(ast/xkeys :return) (ast/immediate f)]))]
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
            (go! @the-env (retwrap form1) conts)
            (print "expected: " )
            (go! @the-env (retwrap form2) conts)
            (println )
            (recur reader)))))))

(def p debug/provenance)

(defn pp [x]
  (:predecessor (p x)))


(defmacro db [x]
  `(binding [debug/*verbose* true] ~x))

(defmacro ddb [x]
  `(binding [debug/*verbose*         true
             debug/*sample-interval* 1000]
     ~x))
