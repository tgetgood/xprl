(ns janus.core
  (:require [clojure.pprint :as pp]
            [janus.ast :as ast]
            [janus.default-env :as default]
            [janus.interpreter :as i]
            [janus.reader :as r]
            [janus.runtime :as rt]
            [taoensso.telemere :as t]))

(def srcpath "../../src/")
(def corexprl (str srcpath "core.xprl"))

(def env (atom default/env))

(def cx (r/file-reader corexprl))

(def s
  (r/string-reader "[0x4e [{:asd 34} [#{:sd 34}]] \n;comment\n #_(f x y [23]) ~(bob x [1 2 3])]"))

(def forms (r/read-file corexprl) )

(def t (r/read (assoc (r/file-reader corexprl) :env @env)))


(def o (atom nil))

(defn loadfile [env fname]
  (let [conts {(ast/keyword "env")    #(reset! env %)
               (ast/keyword "return") #(throw (Exception. %1))}]
    ;; HACK: This repl will not work if we enable multiple executors and work
    ;; stealing. This is because it depends on the order in which the outputs of
    ;; `xprl-def` are executed.
    ;;
    ;; TODO: Rewrite this as soon as statefuls are implemented.
    (letfn [(looper [reader]
              (let [reader (r/read (assoc reader :env @env))
                    result (:result reader)]
                (t/log! {:level :debug
                         :data  (assoc (select-keys (meta result)
                                                    [:file :line :col])
                                       :form result)}
                        "eval form")
                (if (= :eof result)
                  @env
                  (rt/pushngo!
                   i/eval result {}
                   (rt/withcc conts :return
                              (fn [res] (println res) (looper reader)))))))]
      (looper (r/file-reader fname)))))

(defn r [form]
  (rt/pushngo! i/eval form {} {i/return #(reset! o %)}))
