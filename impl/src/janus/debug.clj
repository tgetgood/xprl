(ns janus.debug)

(def ^:dynamic *verbose* false)

;; REVIEW: Why is this a macro?
(defmacro trace! [& args]
  `(when *verbose* (println ~@args "\n")))

(defn provenance [x]
  (::provenance (meta x)))

(defn with-provenance [x p]
  (if (and p (instance? clojure.lang.IMeta x))
    (with-meta x (assoc (meta x) ::provenance p))
    x))

(defn tag [val rule predecessor]
  (with-provenance val {:rule rule :predecessor predecessor}))
