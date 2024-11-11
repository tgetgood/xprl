(ns janus.core
  (:require [janus.reader :as r]))

(def srcpath "../../src/")
(def corexprl (str srcpath "core.xprl"))

(def cx (r/file-reader corexprl))

(def s
  (r/string-reader "[0x4e [{:asd 34} [#{:sd 34}]] \n;comment\n #_(f x y [23]) ~(bob x [1 2 3])]"))

()
