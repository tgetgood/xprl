(ns janus.reader
  "This reader uses the weirdest monadish data pattern, but it seems to work."
  (:refer-clojure :exclude [read meta])
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

(defn meta [r]
  (select-keys r [:string :file :line :col]))

(declare read)

(defn readimmediate [r]
  (update (read r) :result ast/->Immediate))

(defn read-all [r]
  (let [next (read r)]
    (if (= :close (:result next))
      (assoc next :result (:result r))
      (recur (assoc next :result (conj (:result r) (:result next)))))))

(defn read-until [c r]
  (read-all (assoc r :until c :result [])))

(defn readpair [r]
  (let [forms (read-until \) r)
        res   (:result forms)
        n     (count res)]
    (assoc forms :result
           (cond
             (= 1 n) (ast/->Pair (first res) [])

             (= ast/dot (nth res (- n 2)))
             (if (= n 3)
               (ast/->Pair (first res) (last res))
               (ast/->Pair (first res)
                           (into [] (concat (subvec res 1 (- n 2)) (last res)))))

             :else (ast/->Pair (first res) (into [] (rest res)))))))

(defn read1 [s]
  (let [next (.read (:reader s))]
    (if (< next 0)
      (throw (EOFException.))
      (-> s
          (assoc :result (char next))
          (update :col inc)))))

(defn unread1
  [s c]
  (.unread (:reader s) (int c))
  (update s :col dec))

(def delimiters
  #{\( \[ \{ \;})

(defn delimiter? [s]
  (contains? delimiters s))

(defn buildtoken [old]
  (let [new (try (read1 old) (catch EOFException e nil))
        c   (:result new)]
    (cond
      (nil? new) old

      (or (contains? delimiters c) (Character/isWhitespace c)) (unread1 new c)

      (= c (:until old)) (unread1 new c)

      :else (recur (assoc new :token (str (:token old) c))))))

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
      (setcursor (unread1 next c)))))

(defn readtoken [s]
  (-> s
      (assoc :token "")
      consumewhitespace
      (assoc :token "")
      buildtoken))

(def int-pattern
  ;; This is copied from the clojure source (LispReader.java).
  #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")

(defn parse-bool [s]
  (cond
    (= s "true")  ast/t
    (= s "false") ast/f
    :else         nil))

(defn split-symbolic [s]
  (if (str/includes? s ".")
    (str/split s #"\.")
    [s]))

(defn interpret [r]
  (let [s (:token r)]
    ;; FIXME: This is kind of hideous...
    ;; TODO: Read hex, binary, and I suppose octal. I never use octal...
    (if-let [i (parse-long s)]
      i
      (if-let [n (parse-double s)]
        n
        (if-let [b (parse-bool s)]
          b
          (if (str/starts-with? s ":")
            (ast/->Keyword (split-symbolic (apply str (drop 1 s))))
            (if (= s ".")
              ast/dot
              (ast/->Symbol (split-symbolic s)))))))))

(def dispatch
  {
   \( readpair
   ;; \[ readvector
   ;; \{ readmap
   ;; \" readstring
   ;; \# readdispatch
   ;; \; readcomment
   ;; \^ readmeta
   \~ readimmediate})

(defn readinner [r]
  (let [next (read1 r)
        c    (:result next)]
    (cond
      (= c (:until next))    (assoc next :result :close)
      (contains? dispatch c) ((get dispatch c) next)
      :else                  (let [next  (readtoken (unread1 next c))
                                   token (interpret next)]
                               (assoc next :result token)))))

(defn read [r]
  (let [w (consumewhitespace (assoc r :token ""))
        m (meta w)
        o (readinner r)]
    (if (instance? clojure.lang.IObj (:result o))
      (update o :result with-meta m)
      o)))
