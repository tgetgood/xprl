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
  ([ns f] (go! ns f base-conts))
  ([ns f conts]
   (try
     (sys/start! conts (ast/immediate (ns/bind f ns)))
     (catch Throwable e
       (binding [*out* *err*]
         (println e)
         ;;(.printStackTrace e)
         )))))

(defn ev
  ([s] (go! @the-env (:form (r/read (r/string-reader s)))))
  ([s cb] (go! @the-env (:form (r/read (r/string-reader s)))
               (emit/with-return base-conts #(emit/return base-conts (cb %))))))

(defn iev [s]
  (emit/ret-> base-conts
    #(go! @the-env (:form (r/read (r/string-reader s))) %) debug/inspect))

(defn loadfile [envatom fname]
  (println "\nloading:" fname "\n")
  (run! #(go! @envatom % base-conts) (r/read-file fname))
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
  (debug/inspect (:form (r/read (r/string-reader s)))))

(defn test []
  (reload! test-setup)
  (binding [debug/*execution-trace* false]
    (println "\nStarting tests:\n")
    (run! (fn [[test expect]]
            (emit/ret-> base-conts
              (fn [ccs]
                (println "Evaluating: " test)
                (go! @the-env test ccs))
              (fn [result]
                (println "---")
                (when (not= result expect)
                  (println "\033[41m!!!!!!!!!!!!!!!FAILURE!!!!!!!!!!!!\033[0m\n---"))

                (println "result:   " result)
                (println "expected: " expect)
                (println))))
          (partition 2 (r/read-file testxprl)))))

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
  (:form (r/read (r/string-reader s))))
