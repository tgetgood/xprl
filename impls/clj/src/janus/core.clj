(ns janus.core
  (:require [janus.reader :as r]))

(def t
  (r/string-reader "~43"))

(r/read t)
