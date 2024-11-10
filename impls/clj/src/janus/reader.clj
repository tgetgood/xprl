(ns janus.reader
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [janus.ast :as ast])
  (:import [java.io PushbackReader StringReader File FileReader EOFException]))

;; FIXME: Move this out of here once we've tested.
(def srcpath "../../src/")
(def corexprl (str srcpath "core.xprl"))

(defn string-reader [s]
  {:reader (PushbackReader. (StringReader. s))
   :string s
   :line   1
   :col    1})

(defn file-reader [fname]
  {:reader (-> fname File. FileReader. PushbackReader.)
   :file   fname
   :line   1
   :col    1})

(declare read)

(defn readimmediate [r]
  (update (read r) :result ast/->Immediate))


(defn read1 [s]
  (let [next (.read (:reader s))]
    (if (< next 0)
      (throw (EOFException.))
      (-> s
          (assoc :result (char next))
          (update :col inc)))))

(defn unread1 [s c]
  (.unread (:reader s) (int c))
  s)

(def delimiters
  #{\( \[ \{ \;})

(defn delimiter? [s]
  (contains? delimiters s))

(defn buildtoken [old]
  (let [new (read1 old)
        c   (:result new)]
    (if (or (contains? delimiters c) (Character/isWhitespace c))
      (unread1 old c)
      (recur (assoc new :token (str (:token old) c))))))

(defn setcursor [s]
  (let [token (:token s)
        lines (count (filter #(= \newline %) token))
        cols  (count (take-while #(not= \newline %) (reverse token)))]
    (if (= lines 0)
      s ; col has been kept up by read1
      (-> s
          (update :line + lines)
          (assoc :col (inc cols))))))

(defn consumewhitespace [current]
  (let [next (read1 current)
        c    (:result next)]
    (if (Character/isWhitespace c)
      (recur (assoc next :token (str (:token current) c)))
      (setcursor (unread1 current c)))))

(defn readtoken [s]
  (-> s
      (assoc :token "")
      consumewhitespace
      (assoc :token "")
      buildtoken))

;;;;; The following number reading logic copied more or less directly from the
;;;;; clojure source (LispReader.java).

(def int-pattern
  #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")

(def float-pattern
  #"([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

(defn parse-bool [s]
  (cond
    (= s "true")  ast/T
    (= s "false") ast/F
    :else         nil))

(defn split-symbolic [s]
  (if (str/includes? s ".")
    (str/split s #"\.")
    [s]))

(defn interpret [r]
  (let [s (:token r)]
    ;; FIXME: This is kind of hideous...
    (if-let [i (parse-long s)]
      i
      (if-let [n (parse-double s)]
        n
        (if-let [b (parse-bool s)]
          b
          (if (str/starts-with? s ":")
            (ast/->Keyword (split-symbolic (apply str (drop 1 s))))
            (ast/->Symbol (split-symbolic s))))))
))

(def dispatch
  {
   ;; \( readpair
   ;; \[ readvector
   ;; \{ readmap
   ;; \" readstring
   ;; \# readdispatch
   ;; \; readcomment
   ;; \^ readmeta
   \~ readimmediate})

(defn read [r]
  (let [next (read1 r)
        c    (:result next)]
    (cond
      (= c (:until next))    (assoc next :result :close)
      (contains? dispatch c) ((get dispatch c) next)
      :else                  (let [next (readtoken (unread1 r c))
                                   token (interpret next)]
                               (assoc next :result token)))
   )
  )
