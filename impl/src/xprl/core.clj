(ns xprl.core
  (:refer-clojure :exclude [test read-string])
  (:require
   [xprl.ast :as ast]
   [xprl.builtins :as builtins]
   [xprl.debug :as debug]
   [xprl.emission :as emit]
   [xprl.env :as env]
   [xprl.executor :as exec]
   [xprl.interpreter :as i]
   [xprl.ns :as ns]
   [xprl.reader :as r]
   [xprl.system :as sys]))

(def the-env (atom builtins/base-env))

;;;;; UI

(def srcpath "../src/")
(def core [(str srcpath "boot.xprl") (str srcpath "core.xprl")])
(def recxprl (conj core (str srcpath "recur.xprl")))
(def squiggol (conj core (str srcpath "transduction.xprl")))
(def test-setup (conj core (str srcpath "test-setup.xprl")))
(def testxprl (str srcpath "test.xprl"))

(def te (atom nil))

(declare base-conts)

(defn cable [cmap]
  (into {} (map (fn [[k f]] [(ast/xkey k) f])) cmap))

(defn with-return [ccs cb]
  (merge ccs (cable {:return cb})))

(defn env-updater [env]
  (fn [l]
    (reset! te l)
    (let [[sym value] l]
      (assert (ast/symbolic? sym) sym)
      (swap! env ns/ns-intern (ast/symbol sym) value)
      nil)))

(defonce ta (atom nil))
(def base-conts
  (cable {:env     (env-updater the-env)
          :return  (fn [v] (when (not (nil? v)) (println "=>> " v)))
          :unbound #(println "WARNING message on unbound channel:" %)
          :log     #(println "LOG:" %)
          :test    #(reset! ta %)
          :error   #(binding [*out* *err*] (println %))}))

(defn go!
  ([f] (go! f base-conts))
  ([f conts]
   (try
     (sys/start! conts (ast/immediate f))
     (catch Throwable e
       (binding [*out* *err*]
         (println e)
         ;;(.printStackTrace e)
         )))))

(defn ev
  ([s] (go! (:form (r/read (r/string-reader s) @the-env))))
  ([s cb] (go! (:form (r/read (r/string-reader s) @the-env))
               (emit/with-return base-conts #(emit/return base-conts (cb %))))))

;; FIXME: This won't work with cps.
(defn iev [s]
  (debug/inspect (go! (:form (r/read (r/string-reader s) @the-env)))))

(defn loadfile [envatom fname]
  (println "\nloading:" fname "\n")
  (loop [reader (r/file-reader fname)]
    (let [env    @envatom
          reader (r/read reader env)
          form   (:form reader)]
      (if (= :eof form)
        'EOF
        (do
          (go! form base-conts)
          (recur reader)))))
  envatom)

(defn reload! [fnames]
  (reset! the-env builtins/base-env)
  (reduce loadfile the-env fnames)
  :eof)

(defmacro gs [n]
  `(ns/lookup @the-env (ast/symbol ~(clojure.core/name n))))

(defmacro inspect [n]
  `(debug/inspect (gs ~n)))

(defn check [s]
  (debug/inspect (:form (r/read (r/string-reader s) @the-env))))

(defn test []
  ;; FIXME: This testrunner is synchronous, so it cannot test message passing.
  ;; Most of the tests failing as of now need message passing to run.
  (reload! test-setup)
  (binding [debug/*execution-trace* false]
    (let [retwrap (fn [f] (ast/pair (ast/symbol "emit")
                                    [(ast/xkey :return) (ast/immediate f)]))]
      (println "\nStarting tests:\n")
      (loop [reader (r/file-reader testxprl)]
        (let [reader (r/read reader @the-env)
              form1  (:form reader)
              reader (r/read reader @the-env)
              form2  (:form reader)]
          (if (= :eof form1)
            'EOF
            (do
              (emit/ret-> base-conts
                (fn [ccs]
                  (println "Evaluating: " form1)
                  (go! form1 ccs))
                (fn [res]
                  (let [exp form2 #_(go! form2 #_base-conts)]
                    (println "---")
                    (when (not= res exp)
                      (println "\033[41m!!!!!!!!!!!!!!!FAILURE!!!!!!!!!!!!\033[0m\n---"))

                    (println "result:   " res)
                    (println "expected: " exp)
                    (println))))
              (recur reader))))))))

(def p debug/provenance)

(defn pp [x]
  (:predecessor (p x)))


(defmacro db [x]
  `(binding [debug/*verbose* true
             ast/*verbose*   true]
     ~x))

(defmacro ddb [x]
  `(binding [debug/*verbose*         true
             debug/*sample-interval* 1000]
     ~x))

(defn read-string [s]
  (:form (r/read (r/string-reader s) @the-env)))
