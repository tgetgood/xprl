(ns janus.emission
  (:require
   [janus.ast :as ast]
   [janus.runtime :refer [task]]
   [taoensso.telemere :as t]))

(defn event! [id data]
  (t/event! id {:level :trace :data data}))

(defn sanitise [k]
  (assert (ast/keyword? k) "Only xprl keywords can be used in cc maps.")
  k)

(defn sanitise-keys [m]
  (into (empty m) (map (fn [[k v]] [(sanitise k) v])) m))

;; FIXME: This will be a lot less ugly and more extensible if reworked with a protocol.
(defn parse-emission [c [k v]]
  (event! ::parse-raw [k (fn? k) (ast/keyword? k) (contains? c k)])
  (cond
    ;; TODO: What kind of function?
    (fn? k) (do
              (event! ::emit.fn {:fn k :args v})
              (task k v (merge (meta k) {:ch ::fn})))

    (instance? janus.ast.Mu k)
    (do
      (event! :emit.μ {:μ k :args v})
      (task #(i/apply k % {} c) v (merge (meta k) {:ch ::μ})))

    (ast/keyword? k)
    ;; Nested conds. real nice.
    (cond
      (contains? c k)
      (do
        (event! ::emit.bound {:ch-name k :ch (get c k) :msg v})
        (task (get c k) v (merge (dissoc (meta k) :lex) {:ch-name k})))

      (contains? c unbound)
      (do
        (event! ::emit.unbound.caught {:ch-name k :msg v})
        (task (get c unbound) {:ch-name k :msg v}))

      ;; It isn't an error to send a message to nobody, even though that might
      ;; break the system if somebody needs that message.
      ;; REVIEW: What to do about that?
      :else (event! ::emit.unbound.uncaught {:ch-name k :msg v} "Unresolved channel"))

    :else (do
            (t/log! {:level :error
                     :data  [(type k) k]}
                    "Only fns and xprl keywords can be channels.")
            (throw (RuntimeException.
                    (str k " is not a valid channel."))))))

(defn emit
  "For each `kv`, sends `v` to continuation named by `k` in the continuation map
  `c` if `k` is a keyword. If `k` is a function it is assumed to *be* the
  continuation."
  {:style/indent 1}
  [c kvs]
  (event! ::emit-raw kvs)
  (let [tasks (map (partial parse-emission c) kvs)]
    (event! ::emit-tasks tasks)
    (push! tasks)))

(defn withcc
  {:style/indent 1}
  ([c m]
   (merge c (sanitise-keys m)))
  ([c k v & kvs]
   (withcc c (apply hash-map k v kvs))))
