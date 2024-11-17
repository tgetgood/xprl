(ns janus.core
  (:require [clojure.pprint :as pp]
            [janus.ast :as ast]
            [janus.default-env :refer [env]]
            [janus.interpreter :as i]
            [janus.reader :as r]
            [janus.runtime :as rt]
            [taoensso.telemere :as t]))

(def srcpath "../../src/")
(def corexprl (str srcpath "core.xprl"))

(def cx (r/file-reader corexprl))

(def s
  (r/string-reader "[0x4e [{:asd 34} [#{:sd 34}]] \n;comment\n #_(f x y [23]) ~(bob x [1 2 3])]"))

(def forms (r/read-file corexprl))

(def t (r/read (assoc (r/file-reader (str srcpath "test.xprl")) :env env)))


(def o (atom nil))

(defn r [form]
  (rt/pushngo! i/eval form {} {i/return #(reset! o %)}))
