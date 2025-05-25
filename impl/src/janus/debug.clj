(ns janus.debug)

(def ^:dynamic *verbose* false)

;; REVIEW: Why is this a macro?
(defmacro trace! [& args]
  `(when *verbose* (println ~@args)))
