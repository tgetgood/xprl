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
  (let [reader (r/file-reader fname)
        conts  {(ast/keyword "env")    #(do (println %) (reset! env %))
                (ast/keyword "return") #(throw (Exception. %1))}]
    (letfn [(looper []
              (let [form (:result (r/read (assoc reader :env @env)))]
                (t/log! :debug (assoc (select-keys (meta form) [:file :line :col])
                                      :form form))
                (if (= :eof form)
                  @env
                  (rt/pushngo! i/eval form {}
                               (rt/withcc conts :return
                                          (fn [& res] (println res) (looper)))))))]
      (looper))))

(defn r [form]
  (rt/pushngo! i/eval form {} {i/return #(reset! o %)}))
