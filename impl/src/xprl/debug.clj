(ns xprl.debug
  (:require [xprl.ast :refer [inspect]]))

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

;; TODO: Now if I could only reverse these before printing, it would be a lot
;; easier to read...
(defmacro deftracefn [name args & body]
  (let [farg  (first args)
        input (cond
                (symbol? farg) farg
                (map? farg)    (get farg :as)
                true           (assert false))]
    `(defn ~name ~args
       (let [v# (do ~@body)]
         (binding [ast/*verbose* true]
           ;; FIXME: Don't build these strings unless *verbose* is true!
           (trace! "---" ~(str name) "in" (:bindings ~(second args))"---\n"
                   ~input "\n-->\n" v# "\n---"))
         v#))))
