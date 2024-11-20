(ns janus.util
  (:require
   [janus.runtime :as rt]
   [taoensso.telemere :as t]))


(defn form-log! [level form msg]
  (t/log! {:level level
           :data  (assoc (select-keys (meta form) [:string :file :line :col])
                         :form form)}
                msg))

(defn form-error! [form msg]
  (form-log! :error form msg))

(defn fatal-error! [c form ^String msg]
  (form-error! form msg)
  (rt/emit c rt/error {:form form :message msg}))
