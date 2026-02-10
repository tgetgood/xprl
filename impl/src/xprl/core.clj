(ns xprl.core
  (:refer-clojure :exclude [test])
  (:require
   [xprl.ast :as ast]
   [xprl.builtins :as builtins]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.reader :as r]))

(def the-env (atom builtins/base-env))

;;;;; UI

(def srcpath "../src/")
(def core [(str srcpath "boot.xprl") (str srcpath "core.xprl")])
(def recxprl (conj core (str srcpath "recur.xprl")))
(def squiggol (conj core (str srcpath "transduction.xprl")))
(def test-setup (conj core (str srcpath "test-setup.xprl")))
(def testxprl (str srcpath "test.xprl"))

(def te (atom nil))

(defn env-updater [env]
  (fn [l]
    (reset! te l)
    (let [[sym value] (i/interpret l)]
      (assert (ast/symbolic? sym))
      (swap! env env/ns-intern sym value))))

(defn with-return [ccs cb]
  (assoc ccs (ast/xkey :return) cb))

(defn go!
  ([env f] (i/interpret (ast/immediate (env/set-ns env f))))
  ([env f conts] (i/interpret (ast/ctx conts (ast/immediate (env/set-ns env f))))))

(defn evv [s]
  (go! @the-env (:form (r/read (r/string-reader s)))))

(def base-conts
  {(ast/xkey :env)    (env-updater the-env)
   (ast/xkey :return) #(println (i/interpret %))
   (ast/xkey :log) #(println "LOG:" (i/interpret %))
   (ast/xkey :error)  #(binding [*out* *err*]
                          (println %))})
(defn ev [s]
  (go! @the-env (:form (r/read (r/string-reader s))) base-conts))


(defn iev [s]
  (ast/inspect (go! @the-env (:form (r/read (r/string-reader s))))))

(defn loadfile [envatom fname]
  (println "\nloading:" fname "\n")
  (let [conts (merge
               base-conts
               {(ast/xkey :env)    (env-updater envatom)
                (ast/xkey :return) #(throw
                                      (RuntimeException. "return to top level!"))})]
    (loop [reader (r/file-reader fname)]
      (let [reader (r/read reader)
            env    @envatom
            form   (:form reader)]
        (if (= :eof form)
          'EOF
          (do
            (go! @envatom form (with-return conts println))
            (recur reader))))))
  envatom)

(defn reload! [fnames]
  (reset! the-env builtins/base-env)
  (reduce loadfile the-env fnames)
  :eof)

(defmacro gs [n]
  `(env/lookup @the-env (ast/symbol ~(clojure.core/name n))))

(defmacro inspect [n]
  `(ast/inspect (gs ~n)))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @the-env))))

(defn test []
  (reload! test-setup)
  (binding [debug/*execution-trace* false ]
    (let [retwrap (fn [f] (ast/pair (ast/symbol "emit")
                                    [(ast/xkey :return) (ast/immediate f)]))]
      (println "\nStarting tests:\n")
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
              (print "result:   ")
              (go! @the-env (retwrap form1) base-conts)
              (print "expected: " )
              (go! @the-env (retwrap form2) base-conts)
              (println )
              (recur reader))))))))

(def p debug/provenance)

(defn pp [x]
  (:predecessor (p x)))


(defmacro db [x]
  `(binding [debug/*verbose* true] ~x))

(defmacro ddb [x]
  `(binding [debug/*verbose*         true
             debug/*sample-interval* 1000]
     ~x))
