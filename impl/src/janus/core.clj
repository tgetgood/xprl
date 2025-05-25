(ns janus.core
  (:refer-clojure :exclude [test])
  (:require
   [janus.ast :as ast]
   [janus.env :as env]
   [janus.interpreter :as i]
   [janus.reader :as r]
   [janus.runtime :as rt]))

;;;;; Builtins

(def macros
  {"μ"      i/createμ
   ;;   "ν"       createν
   "emit"   i/emit
   "select" i/select
   ;; "first*" first*
   ;; "rest*"  rest*
   })

(defn nth* [c i]
  (nth c (dec i)))

(def fns
  {"+*"   +
   "**"   *
   "-*"   -
   "/*"   /
   ">*"   >
   "<*"   <
   "=*"   =
   "mod*" mod

   "first*" first
   "rest*"  #(into [] (rest %)) ; We don't deal with seqs

   "count*" count
   "nth*"   nth* ; Base 1 indexing
   })

(defn tagged [f]
  (fn [[k v]]
    (let [k' (ast/symbol k)]
      [k' (f (with-meta v {:name k'}))])))

(defn tag-primitives [x f]
  (into {} (map (tagged f)) x))

(def base-env
  (reduce (fn [acc [sym val]] (env/ns-bind acc sym val)) (env/empty-ns)
          (concat
           (map (tagged ast/macro) macros)
           (map (tagged ast/pfn) fns))))

;;;;; UI

(def srcpath "../src/")
(def recxprl (str srcpath "recur.xprl"))
(def core (str srcpath "core.xprl"))
(def testxprl (str srcpath "test.xprl"))

(defn go!
  ([env f]
   (i/walk (env/with-env (ast/immediate f) env)))
  ([env f ccs]
   (rt/schedule [(fn [_] (rt/connect (go! env f) ccs))])
   (rt/run!)))

(defn ev [s]
  (go! @env/env (:form (r/read (r/string-reader s)))))

(defn iev [s]
  (ast/inspect (ev s)))

(defn loadfile [envatom fname]
  (let [conts {(ast/xkeys :env)    (fn [[sym value]] (swap! envatom env/ns-bind sym value))
               (ast/xkeys :return) #(throw (RuntimeException. "return to top level!"))
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
  (alter-var-root #'env/env (constantly (atom base-env)))
  (loadfile env/env fname))

(defmacro inspect [n]
  `(-> @env (get-in [:names (ast/symbol ~(clojure.core/name n))]) ast/inspect))

(defn el [form name]
  (env/lookup (env/get-env form) (ast/symbol name)))

(defn check [s]
  (ast/inspect (:form (r/read (r/string-reader s) @env/env))))

(defn test []
  (let [conts {(ast/xkeys :env) (fn [[sym value]] (swap! env/env env/ns-bind sym value))}]
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
            (go! @env/env form1 (rt/with-return conts println))
            (print "expected: " )
            (go! @env/env form2 (rt/with-return conts println))
            (recur reader)))))))
