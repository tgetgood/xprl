(ns janus.reader
  "This reader uses the weirdest monadish data pattern, but it seems to work."
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [clojure.set :as s]
            [janus.ast :as ast]
            [janus.debug :as debug])
  (:import [java.io PushbackReader StringReader File FileReader EOFException]))

(defn string-reader [^String s]
  {:reader (PushbackReader. (StringReader. s))
   :origin ::string
   :string s
   :until  '()
   :line   1
   :col    1})

(defn file-reader [^String fname]
  {:reader (-> fname File. FileReader. PushbackReader.)
   :origin ::file
   :file   fname
   :until  '()
   :line   1
   :col    1})

(defn stdin-reader []
  {:reader *in*
   :origin ::stdin
   :until  '()
   :col    1
   :line   1})

(defn clean-meta [r]
  (dissoc r :token :until :result :reader :gensyms))

(defn read1 [s]
  (let [next (.read ^PushbackReader (:reader s))]
    (if (< next 0)
      nil
      (-> s
          (assoc :result (char next))
          (update :col inc)))))

(defn unread1
  [s c]
  (.unread ^PushbackReader (:reader s) (int c))
  (update s :col dec))

(declare read*)

(def delimiters
  #{\( \[ \{ \;})

(defn delimiter? [s]
  (contains? delimiters s))

(defn buildtoken [old]
  (let [new     (read1 old)
        ^char c (:result new)]
    (cond
      (nil? new) (assoc old :result :eof)

      (or (contains? delimiters c) (Character/isWhitespace c)) (unread1 new c)

      (and (seq (:until new)) (= c ^char (first (:until new)))) (unread1 new c)

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
  (let [next    (read1 current)
        ^char c (:result next)]
    (cond
      (nil? next) (assoc current :result :eof)

      (Character/isWhitespace c)
      (recur (assoc next :token (str (:token current) c)))

      :else (setcursor (unread1 next c)))))

(defn readtoken [s]
  (-> s
      (assoc :token "")
      consumewhitespace
      (assoc :token "")
      buildtoken))

(defn parse-number [s]
  (cond
    (= s "0")        0
    (= \0 (first s)) (case (second s)
                       \x (Long/parseLong (subs s 2) 16)
                       \b (Long/parseLong (subs s 2) 2)
                       (Long/parseLong (subs s 1) 8))
    :else            (parse-long s)))

(defn parse-keyword [s]
  (when (str/starts-with? s ":")
    (ast/keyword (subs s 1))))

(defn parse-bool [s]
  (cond
    (= s "true")  true
    (= s "false") false
    :else         nil))

(defn first-to-pass [s & fs]
  (loop [[f & fs] fs]
    (when f
      (let [v (f s)]
        (if (nil? v)
          (recur fs)
          v)))))

(defn parse-symbol [{:keys [token gensyms] :as r}]
  ;; reader gensyms are global to the form being read but unique between forms.
  ;; REVIEW: I'm not clear that we *need* gensyms for hygiene. In fact my
  ;; thinking is that if they're needed, that's an indication something is wrong
  ;; with the context handling logic.
  ;;
  ;; But I'm not sure yet, so we still have them.
  (if (str/ends-with? token "#")
    (let [s (apply str (butlast token))]
      (if-let [sym (get @gensyms s)]
        sym
        (let [sym (ast/symbol (name (gensym (str s "_"))))]
          (swap! gensyms assoc s sym)
          sym)))
    (ast/symbol token)))

(defn interpret [r]
  (let [s (:token r)]
    (let [t (first-to-pass s parse-number parse-double parse-bool parse-keyword)]
      (if (nil? t)
        (parse-symbol r)
        t))))

(defn readimmediate [r]
  (update (read* r) :result ast/immediate))

(defn read-all [r]
  (let [next (read* r)]
    (if (or (= :close (:result next)) (= :eof (:result next)))
      (assoc next :result (:result r))
      (recur (assoc next :result (conj (:result r) (:result next)))))))

(defn read-until [c r]
  (read-all (-> r (update :until conj c) (assoc :result []))))

(defn readpair [r]
  (let [forms (read-until \) r)
        res   (:result forms)
        n     (count res)]
    (assoc forms :result
           (cond
             (= 1 n) (ast/pair (first res) (debug/with-provenance (ast/list [])
                                             (clean-meta r)))

             (= (ast/symbol ".") (nth res (- n 2)))
             (if (= n 3)
               (ast/pair (first res) (last res))
               (ast/pair (first res)
                         (debug/with-provenance
                           (ast/list
                            (concat (subvec res 1 (- n 2)) (last res)))
                           (debug/provenance (second res)))))

             :else (ast/pair (first res) (debug/with-provenance
                                           (ast/list (rest res))
                                           (debug/provenance (second res))))))))

(defn readvector [r]
  (update (read-until \] r) :result #(debug/with-provenance (ast/list %)
                                       (debug/provenance r))))

(defn readset [r]
  (let [forms (read-until \} r)]
    (update forms :result set)))

(defn readmap [r]
  (let [next (read-until \} r)
        m (into {} (partition-all 2) (:result next))]
    (assoc next :result m)))

(defn readdiscard [r]
  (dissoc (read* r) :result))

(defn readlinecomment [r]
  (let [next (read1 r)]
    (cond
      (nil? next) (assoc r :result :eof)

      (contains? #{\return \newline} (:result next))
      (-> next (dissoc :result) (update :line inc) (assoc :col 1))

      :else       (recur next))))

(defn read-unicode [r]
  ;; FIXME:
  (throw (Exception. "Not implemented")))

(defn read-unicode-octal [r]
  ;; FIXME:
  (throw (Exception. "Not implemented")))

(defn read-special [r]
  (let [next (read1 r)]
    (case (:result next)
      \t (assoc next :result \tab)
      \r (assoc next :result \return)
      \n (assoc next :result \newline)
      \\ next
      \b (assoc next :result \backspace)
      \f (assoc next :result \formfeed)
      \u (read-unicode next)
      (if (Character/isDigit ^char (:result next))
        (read-unicode-octal next)
        (throw (RuntimeException.
                (str "Invalid char escape: \\" (:result next))))))))

(defn readstring [r]
  (loop [sb     (StringBuilder.)
         reader r]
    (let [next (read1 reader)
          c    (:result next)]
      (cond
        (nil? c) (throw (RuntimeException. "EOF while reading string."))
        (= c \") (assoc next :result (.toString sb))
        (= c \\) (let [next (read-special next)]
                   (recur (.append sb (:result next)) next))
        :else    (recur (.append sb c) (setcursor (assoc next :token (str c))))))))

(def subdispatch
  {\{ readset
   \_ readdiscard})

(defn readdispatch [r]
  (let [next (read1 r)
        c (:result next)]
    ((get subdispatch c #(throw (Exception. (str "invalid reader macro #" c))))
     next)))

(def dispatch
  {\( readpair
   \[ readvector
   \{ readmap
   \" readstring
   \# readdispatch
   \; readlinecomment
   \~ readimmediate})

(defn readinner [r]
  (let [next (read1 r)
        c    (:result next)]
    (cond
      (nil? next) (assoc r :result :eof)

      (= c (first (:until next)))
      (-> next (assoc :result :close) (update :until rest))

      (contains? dispatch c) ((get dispatch c) next)

      :else (let [next  (readtoken (unread1 next c))
                  token (interpret next)]
              (assoc next :result token)))))

(defn read*
  [r]
  (let [w (consumewhitespace (assoc r :token ""))
        m (clean-meta w)
        o (readinner w)]
    (if (not (contains? o :result))
      (recur o)
      (update o :result debug/with-provenance m))))

(defn read [reader]
  (s/rename-keys (read* (assoc reader :gensyms (atom {}))) {:result :form}))

(defn read-file
  "Reads all forms from file `fname` and returns then in a vector.
  Note that this does not set the context and so the returned forms cannot be
  evaluated if they contain any unbound symbols.
  Only really useful for testing the reader."
  [fname]
  (loop [results []
         reader  (file-reader fname)]
    (let [next (read* reader)
          result  (:result next)]
      (if (= :eof result)
        results
        (recur (conj results result) next)))))
