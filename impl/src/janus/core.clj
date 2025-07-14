(ns janus.core
  (:refer-clojure :exclude [test])
  (:require
   [janus.ast :as ast]
   [janus.env :as env]
   [janus.debug :as debug]
   [janus.interpreter :as i]
   [janus.reader :as r]
   [janus.runtime :as rt]))

;;;;; Builtins

(defn fn-reduced? [args]
  (every? i/evaluated? args))

(defn primitive [p f]
  (ast/primitive p (with-meta #(apply f %) (meta f))))

(defn primitives [p m]
  (reduce (fn [acc [k v]] (assoc acc (ast/symbol k) (primitive p v))) {} m))

(defn macros [m]
  (reduce (fn [acc [k [p f]]]
            (assoc acc (ast/symbol k) (ast/primitive p f))) {} m))

(def special
  "Things that would traditionally be special forms."
  (macros
   {"μ"      [i/μ-ready? #'i/μ]
    "ν"      [i/μ-ready? #'i/ν]
    "emit"   [(constantly true) #'i/emit]
    "select" [i/check-select #'i/select]

    "seq*"  [(constantly true) #'ast/seq]
    "conc*" [(constantly true) #'ast/conc]

    ;; "first*" first*
    ;; "rest*"  rest*
    }))

(defn nth* [c i]
  (nth c (dec i)))

(defn rest* [xs]
  (into [] (rest xs)))

(defn empty?* [x]
  (boolean (empty? x)))

(defn not* [x]
  (assert (boolean? x))
  (not x))

(def fns
  (primitives
   fn-reduced?
   ;; Using vars lets us pass metadata downstream for debugging.
   {"+*"   #'+
    "**"   #'*
    "-*"   #'-
    "/*"   #'/
    ">*"   #'>
    "<*"   #'<
    "=*"   #'=
    "mod*" #'mod
    "not*" #'not*
    "str*" #'str

    "list?*"  #'ast/list?
    "map?*"   #'map?
    "merge*"  #'merge
    "empty?*" #'empty?*

    "symbol?*" #'ast/symbol?

    "first*" #'first
    "rest*"  #'rest*

    "count*" #'count
    "nth*"   #'nth* ; Base 1 indexing

    "connect*" #'rt/connect
    }))

(def base-env
  (reduce (fn [e [k v]] (env/bind* e k v)) env/empty-ns (merge special fns)))

(def the-env (atom base-env))

;;;;; UI

(def srcpath "../src/")
(def recxprl (str srcpath "recur.xprl"))
(def core (str srcpath "core.xprl"))
(def td (str srcpath "base-transduction.xprl"))
(def testxprl (str srcpath "test.xprl"))

(defn go!
  ([env f]
   (i/walk* env (debug/with-provenance (ast/immediate f)
                 {:origin ::repl :predecessor f})))
  ([env f ccs]
   (rt/schedule (ast/list [(fn [_] (rt/connect (go! env f) ccs))]))
   (rt/run!)))

(defn ev [s]
  (let [conts {(ast/xkeys :env)    (fn [l]
                                     (let [[sym value] l]
                                       (swap! the-env env/bind* sym value)))
               (ast/xkeys :return) println
               (ast/xkeys :error)  (fn [x]
                                     (println "Error: " x))}]
    (go! @the-env (:form (r/read (r/string-reader s))) conts)))


(defn iev [s]
  (ast/inspect (go! @the-env (:form (r/read (r/string-reader s))))))

(defn loadfile [envatom fname]
  (let [conts {(ast/xkeys :env)    (fn [l]
                                     (let [[sym value] l]
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
  (reset! the-env base-env)
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
