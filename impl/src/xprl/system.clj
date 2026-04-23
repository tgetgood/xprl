(ns xprl.system
  (:refer-clojure :exclude [resolve])
  (:require [clojure.pprint :refer [pprint]]
            [xprl.ast :as ast]))

(def ret (ast/xkey :return))

(def empty-executor
  {:stack      []
   :sv-index   {}
   :sv-cache   {}})

(defn empty-ctx
  ([] empty-executor)
  ([ccmap] (assoc empty-executor :ccmap ccmap)))

(defn index-task [index {:keys [waiting] :as task}]
  (reduce (fn [entry sv] (update entry sv (fnil conj #{}) task)) index waiting))

(defn enqueue [ctx task]
  ;; (println "queuing" (dissoc task :ctx))
  (let [task (update task :ctx #(merge (:ctx ctx) %))
        wait-points (ast/svs task)]
    (if (empty? wait-points)
      (update ctx :stack conj task)
      (let [task (assoc task :waiting wait-points)]
        (update ctx :sv-index index-task task)))))

(defn net {:style/indent 1} [ctx & tasks]
  (reduce enqueue ctx tasks))

(defn merge-ctx [root μ]
  (assert (empty? (:state μ)))
  (-> root
      (update :sv-index merge (:sv-index μ))
      (update :sv-cache merge (:sv-cache μ))))

(defn capture [ctx]
  (let [queue (atom (clojure.lang.PersistentQueue/EMPTY))]
    {:queue queue
     :ctx   (assoc ctx :ccmap {::capture (fn [x] (swap! queue conj x))})}))

(defn captured? [ctx]
  (contains? (:ccmap ctx) ::capture))

(defn uncapture [ctx]
  (update ctx :ccmap dissoc ::capture))

;;;;; Message passing
;;
;; Why is this so complicated?

(defn update-task-index [index wp old new]
  (-> index
      (update wp disj old)
      (update wp conj new)))

(defn deliver-task [task ch val]
  (-> task
      (update :waiting disj ch)
      (update :args #(into [] (map (fn [x] (if (= x ch) val x))) %))))

(defn reindex-task [ch val]
  (fn [ctx task]
    (let [new (deliver-task task ch val)
          wps (:waiting new)]
      (if (empty? wps)
        (update ctx :stack conj new)
        (update ctx :sv-index #(reduce (fn [index wp]
                                         (update-task-index index wp task new))
                                       % wps) )))))

(defn deliver-val [ctx ch val]
  (let [tasks (get-in ctx [:sv-index ch])
        ctx   (-> ctx
                  (update :sv-cache assoc ch val)
                  (update :sv-index dissoc ch))]
    (reduce (reindex-task ch val) ctx tasks)))

(defn try-deliver [ctx ch val]
  (cond
    (contains? (:sv-index ctx) ch) (deliver-val ctx ch val)
    (contains? ctx :parent)        (update ctx :parent try-deliver ch val)
    true
    (binding [*out* *err*]
      (println "WARNING: message" val "cannot be delivered to" ch
               "because the destination is unbound. Dropping message.")
      (update ctx :sv-cache assoc ch val))))

(defn deliver! [ctx ch val]
  (assert (ast/sv? ch) (str "only single sync points are implemented: " (type ch)))
  (try-deliver ctx ch val))

(defn find-in-ccmap [ctx key]
  (when ctx
    ;; Stop traversing if we encounter a capture.
    (or (get (:ccmap ctx) key)
        (when-let [ch (get (:ccmap ctx) ::capture)]
          (with-meta ch {:captured true}))
        (recur (:parent ctx) key))))

(defn resolve [ctx key]
  (or (get (:ctx ctx) key) (find-in-ccmap ctx key)))

(defn send! [exec [key val]]
  (let [ch (resolve exec key)]
    ;; (println "sending" key ch val)
    ;; (pprint exec)
    (cond
      ;; FIXME: This is ugly.
      (:captured (meta ch)) (do (ch [key val]) exec)

      ;; interface with clj
      (fn? ch)  (do (ch val) exec)
      (nil? ch) (let [e   (ast/xkey :error)
                      ech (resolve exec e)]
                  (println "error ch" ech)
                  (if ech
                    ;; FIXME: We need to check for the special "unbound" channel.
                    ;; That's the ticket to capture.
                    (send! exec [e ["msg on unbound channel" [key val]]])
                    (throw (RuntimeException.
                            (str "msg on unbound channel: " key " <- " val)))))
      true      (deliver! exec ch val))))

(defn return [ctx v]
  (net ctx
    {:call send!
     :args [ret v]}))

(defn return-first [ctx [v]]
  (return ctx v))

;;;;; Execution
;;
;; Basically this moves the computation one step forward so long as there's work
;; that can presently be done.
;;
;; TODO: Scan the work context looking for calls that can't be compiled for
;; native excution. If there aren't any, then we're ready to emit target code.
;;
;; REVIEW: This is a somewhat sloppy way to do things, but it might work as a
;; first go.
;;
;; Also note that running until the stack is empty before emitting target code
;; is pretty much just constant folding and so should be preferred.

(defn stalled? [ctx]
  (empty? (:stack ctx)))

(defn run1
  "Takes a computation described as data (a `ctx` map), runs the first task off
  of the work stack and returns the remaining computation."
  [exec]
  (let [task (peek (:stack exec))
        exec (update exec :stack pop)
        ctx  (:ctx task)]
    ;; REVIEW: I shouldn't need to clear ccmap here as well, should I?
    ((:call task) (assoc exec :ctx ctx) (:args task))))

(defn run* [ctx]
  (if (stalled? ctx)
    (if (and (empty? (:sv-index ctx)) (contains? ctx :parent))
      ;; Sub computation finished, continue with parent.
      (do
        ;; (println "jumping to parent")
        ;; (pprint ctx)
        (recur (update (:parent ctx) :sv-cache merge (:sv-cache ctx))))
      ctx)
    (recur (run1 ctx))))

(defn run [n ctx]
  (if (or (zero? n) (stalled? ctx))
    ctx
    (recur (dec n) (run1 ctx))))
