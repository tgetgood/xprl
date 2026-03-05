(ns xprl.core
  (:refer-clojure :exclude [test read-string])
  (:require
   [xprl.ast :as ast]
   [xprl.builtins :as builtins]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.ns :as ns]
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
    (let [[sym value] l]
      (assert (ast/symbolic? sym))
      (swap! env ns/ns-intern (ast/symbol sym) value))))

(defn with-return [ccs cb]
  (assoc ccs (ast/xkey :return) cb))

(defn go!
  ([f] (i/walk {} {} (ast/immediate f)))
  ([f conts] (i/walk {} {:ctx conts} (ast/immediate f))))

(defn evv [s]
  (go! (:form (r/read (r/string-reader s) @the-env))))

(def base-conts
  {(ast/xkey :env)     (env-updater the-env)
   (ast/xkey :return)  (fn [v] (when (not (nil? v)) (println v)))
   (ast/xkey :unbound) #(println "WARNING message on unbound channel:" %)
   (ast/xkey :log)     #(println "LOG:" %)
   (ast/xkey :error)   #(binding [*out* *err*]
                         (println %))})
(defn ev [s]
  (go! (:form (r/read (r/string-reader s) @the-env)) base-conts))


(defn iev [s]
  (ast/inspect (go! (:form (r/read (r/string-reader s) @the-env)))))

(defn loadfile [envatom fname]
  (println "\nloading:" fname "\n")
  (let [conts (merge base-conts {(ast/xkey :env) (env-updater envatom)})]
    (loop [reader (r/file-reader fname)]
      (let [env    @envatom
            reader (r/read reader env)
            form   (:form reader)]
        (if (= :eof form)
          'EOF
          (do
            (go! (ast/emission [[(ast/xkey :return) (ast/immediate form)]]) conts)
            (recur reader))))))
  envatom)

(defn reload! [fnames]
  (reset! the-env builtins/base-env)
  (reduce loadfile the-env fnames)
  :eof)

(defmacro gs [n]
  `(ns/lookup @the-env (ast/symbol ~(clojure.core/name n))))

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
        (let [reader (r/read reader @the-env)
              form1  (:form reader)
              reader (r/read reader @the-env)
              form2  (:form reader)]
          (if (= :eof form1)
            'EOF
            (do
              (println "Evaluating: " form1)
              (let [res (go! form1 base-conts)
                    exp (go! form2 base-conts)]
                (println "---")
                (when (not= res exp)
                  ;; TODO: colour.
                  (println "!!!!!!!!!!!!!!!FAILURE!!!!!!!!!!!!\n---"))
                (println "result:   " res)
                (println "expected: " exp)
                (println)
                (recur reader)))))))))

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
