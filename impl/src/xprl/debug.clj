(ns xprl.debug)

(def ^:dynamic *verbose* false)

(def ^:dynamic *sample-interval*
  "Only show one trace each interval. A form of rate limiting."
  0)

(def counter (atom 0))

(defn print! [args]
  (apply println args)
  (println))

(defn trace! [& args]
  (when *verbose*
    (swap! counter inc)
    (if (< 0 *sample-interval*)
      (when (= 0 (mod @counter *sample-interval*))
        (print! args))
      (print! args))))

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
