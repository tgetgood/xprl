(ns xprl.core
  (:refer-clojure :exclude [test read-string])
  (:require
   [xprl.ast :as ast]
   [xprl.builtins :as builtins]
   [xprl.debug :as debug]
   [xprl.env :as env]
   [xprl.interpreter :as i]
   [xprl.ns :as ns]
   [xprl.reader :as r]
   [xprl.rt :as rt]))

(def the-env (atom builtins/base-env))

(rt/init!)

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
          :return  (fn [v] (when (not (nil? v)) (println "\n=>> " v)))
          :unbound #(println "WARNING message on unbound channel:" %)
          :log     #(println "LOG:" %)
          :test    #(reset! ta %)
          :error   #(binding [*out* *err*] (println %))}))

(defn go!
  ([ns f] (go! ns f base-conts))
  ([ns f conts]
   (try
     (rt/seed! conts (fn [conts] (i/walk conts (ast/immediate (ns/bind f ns)))))
     (catch Throwable e
       (binding [*out* *err*]
         (println e)
         ;;(.printStackTrace e)
         )))))

(def *x)

(defn ev
  ([s] (go! @the-env (:form (r/read (r/string-reader s)))
            (rt/with-return base-conts
              #(do (alter-var-root #'*x (constantly %)) (rt/return base-conts %)))))
  ([s cb] (go! @the-env (:form (r/read (r/string-reader s)))
               (rt/with-return base-conts #(rt/return base-conts (cb %))))))

(defn iev [s]
  (rt/ret-> base-conts
    #(go! @the-env (:form (r/read (r/string-reader s))) %) debug/inspect))

(defn load-seq [envatom forms cb]
  (if (seq forms)
    (go! @envatom (first forms) (rt/with-return base-conts
                                  (fn [res]
                                    (rt/return base-conts res)
                                    (load-seq envatom (rest forms) cb))))
    (cb)))

(defn loadfile [envatom fname cb]
  (println "\nloading:" fname "\n")
  (load-seq envatom (r/read-file fname) cb))

(defn reload-1 [fnames cb]
  (if (seq fnames)
    (loadfile the-env (first fnames) (fn [] (reload-1 (rest fnames) cb)))
    (cb)))

(defn reload! [fnames & [after]]
  (reset! the-env builtins/base-env)
  (reload-1 fnames (or after (fn [] (println :eof)))))

(defmacro gs [n]
  `(ns/lookup @the-env (ast/symbol ~(clojure.core/name n))))

(defmacro inspect [n]
  `(debug/inspect (gs ~n)))

(defn check [s]
  (debug/inspect (:form (r/read (r/string-reader s)))))

(defn run-tests! [tests acc]
  (if (seq tests)
    (let [[test expect] (first tests)]
      (rt/ret-> base-conts
        (fn [ccs]
          (println "Evaluating: " test)
          (go! @the-env test ccs))
        (fn [result]
          (println "---")
          (when (not= result expect)
            (println "\033[41m!!!!!!!!!!!!!!!FAILURE!!!!!!!!!!!!\033[0m\n---"))

          (println "result:   " result)
          (println "expected: " expect)
          (println)
          (run-tests! (rest tests) (update acc (if (= result expect) :pass :fail) inc)))))
    ;; TODO: colour "Finished" red or green depending on if anything failed or not.
    ;; Good UI is about the little things
    (println "Finished\n----------\nPassed:" (get acc :pass) "\nFailed:" (get acc :fail))))

(defn test []
  (reload! test-setup
           (fn []
             (binding [debug/*execution-trace* false]
               (let [tests (partition 2 (r/read-file testxprl))]
                 (println "\nRunning" (count tests) "tests:\n==================\n")
                 (run-tests! tests {:pass 0 :fail 0}))))))

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
