(ns xprl.debug
  (:require [xprl.ast :as ast]))

(def ^:dynamic *verbose* false)

(def ^:dynamic *sample-interval*
  "Only show one trace each interval. A form of rate limiting."
  0)

(def counter (atom 0))

(defn print! [args]
  (apply println args)
  (println))

(defmacro trace! [& args]
  `(when *verbose*
     (apply println [~@args])
     (println)))

(defn provenance [x]
  (::provenance (meta x)))

(defn with-provenance [x p]
  (if (and p (instance? clojure.lang.IMeta x))
    (with-meta x (assoc (meta x) ::provenance p))
    x))

(defn tag [val rule predecessor]
  (with-provenance val {:rule rule :predecessor predecessor}))

;; storage location for errors when invoking clj externals. akin to *e
(defonce *pfn (atom nil))

(def ^:dynamic *execution-trace* true)

(defonce index (atom {}))
(def ^:dynamic *index-key* :master)

(defn record! [in out md]
  (when-not (or (nil? out) (= in out))
    (swap! index update *index-key* update out (fnil conj #{})
           (with-meta in md))))

(defn clear-index! []
  (reset! index {}))

(defmacro with-key [k body]
  `(binding [*index-key* ~k]
     ~body))

(defn causes
  ([form] (causes *index-key* form))
  ([k form] (get-in @index [k form])))

;; TODO: Now if I could only reverse these before printing, it would be a lot
;; easier to read...
(defmacro deftracefn [name args & body]
  (let [farg  (first args)
        input (cond
                (symbol? farg)  farg
                (ast/map? farg) (get farg :as)
                true            (assert false))]
    `(defn ~name ~args
       (let [v# (do ~@body)]
         (when *execution-trace*
           (record! ~input v# {:op ~(keyword name)}))
         (trace! "---" ~(str name) "with" ~(if (ast/map? (second args))
                                             (:as (second args))
                                             (second args))
                 "\n---\n" ~input "\n-->\n" v# "\n---")
         v#))))

;; REVIEW: A more useful debugging tool might be to store a map of all
;; transitions that occur during interpretation.
;;
;; Interpretation isn't actually an ordered process. It's a set of
;; (theoretically) reversible transformations that we search until we reach a
;; value. Thus printing out the sequence of things that happen is misleading in
;; some ways.
;;
;; Of course it sometimes helps to be able to trace the execution of the current
;; implementation, but that could likely be better accomplished by keeping the
;; set of all transforms performed and just following the paths in which we're
;; interested.
