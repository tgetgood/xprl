(ns xprl.debug
  (:require [xprl.ast :as ast]
            [xprl.env :as env]))

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

;; This noticably slows down execution, so let's not leave in on by default for
;; now because, frankly, it doesn't carry its own weight yet. Maybe that will
;; change as I learn to use it.
(def ^:dynamic *execution-trace* false)

(defonce index (atom {}))
(def ^:dynamic *index-key* :master)

(defn record! [in out md]
  (when-not (or (nil? out) (= in out))
    (swap! index update *index-key* update out (fnil conj #{})
           (with-meta in md))))

(defn clear-index! []
  (reset! index {}))

(defmacro with-key [k body]
  `(binding [*index-key*       ~k
             *execution-trace* true]
     ~body))

(defn causes
  ([form] (causes *index-key* form))
  ([k form] (get-in @index [k form])))

(defn build-env [form]
  (cond
    (ast/immediate? form)   (build-env (:form form))
    (ast/application? form) (merge (build-env (:head form)) (build-env (:tail form)))
    (ast/pair? form)        (merge (build-env (:head form)) (build-env (:tail form)))
    (ast/μ? form)           (build-env (:body form))
    (ast/emission? form)    (build-env (:msgs form))
    (ast/coll? form)        (reduce merge {} (map build-env form))
    (ast/input? form)       (merge
                             (if (env/bound? form) (build-env (:binding form)) {})
                             {form (if (env/bound? form) (env/binding form) :unbound)})
    true                    {}))

;; TODO: Now if I could only reverse these before printing, it would be a lot
;; easier to read...
(defmacro deftracefn [name args & body]
  (let [env   (first args)
        input (if (= 2 (count args)) (second args) (into [] (rest args)))]
    `(defn ~name ~args
       (let [v# (do ~@body)]
         (when *execution-trace*
           (record! ~input v# {:op ~(keyword name)}))
         (trace! "---" ~(str name)
                 "in"  (build-env ~input)
                 "\n---\n" ~input "\n-->\n"
                 ;; (build-env v#) "\n--\n"
                 v# "\n---")
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
