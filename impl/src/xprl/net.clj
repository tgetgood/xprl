;; REVIEW: I know this is throwaway code, but it's really bad, even for that.
;; I'm not going to be able to read this in a week, let alone debug it.
;;
;; But at least it's out of my head and on paper...
(ns xprl.net
  (:refer-clojure :exclude [resolve eval apply delay run!])
  (:require [xprl.ast :as ast]
            [xprl.env :as env]))

;; A SyncVal is basically a pipe that can only ever receive one element and so
;; the stream end is just a value. I don't see a benefit here of separating the
;; read and write ends, so we just use one value.
;;
;; REVIEW: I don't want the api to be too different re pipes.
(defrecord SyncVal [key])

(defn sv
  ([] (sv (gensym "sync-val-")))
  ([key] (->SyncVal key)))

(defn sv? [x]
  (instance? SyncVal x))

(def ret (ast/xkey :return))

;; (defn stream-value [key n]
;;   {:node :stream-val
;;    :key  key
;;    :n    n})

(defn pipe [& [n]]
  (let [key (gensym "pipe")
        ch  {:node :channel
             :key  key}]
    [ch (map (fn [i] (sv (str key "-" i))) (map inc (range)))]))

(def empty-executor-state
  {:stack      []
   :sv-index   {}
   :sv-check   {}
   :gen        0})

;; Creates an exector. Can be used to coordination an entire cpu core or any
;; subcomputation on one. There is intentionally no distinction between sub and
;; super executors. An executor can never know if it's at the top because there
;; is no top. In principle.
(defn executor []
  ;; N.B.: This is explicitly intended to be dealt with in a single thread.
  (atom empty-executor-state))

(defn svs [task]
  (into #{} (filter sv?) (:args task)))

;; REVIEW: Do I need the generation number? or is it just habit?
;;
;; It enforces linearity which could slow things down for no reason.
;; Maybe I should go for Refs instead.
(defn tx! {:style/indent 1} [exec old new]
  (compare-and-set! exec old (assoc new :gen (inc (:gen old)))))

(defn txf! [exec f]
  (swap! exec #(update (f %) :gen inc)))


(defn update-task-index [index wp old new]
  (-> index
      (update wp disj old)
      (update wp conj new)))

(defn deliver-task [task ch val]
  (-> task
      (update :waiting disj ch)
      (update :args #(into [] (map (fn [x] (if (= x ch) val x))) %))))

(defn reindex-task [ch val]
  (fn [work task]
    (let [t' (deliver-task task ch val)
          wps (:waiting t')]
      ;; (println "reindexing:" (dissoc task :ctx) "->-" (dissoc t' :ctx))
      (if (seq wps)
        (update work :sv-index #(reduce (fn [index wp]
                                       (update-task-index index wp task t'))
                                     % wps))
        (update work :stack conj t')))))

(defn deliver! [exec ch val]
  (assert (sv? ch) (str "only single sync points are implemented: " (type ch)))
  (assert (not (contains? (:sv-check @exec) ch))
          (str "Attempting to deliver second message to singlular value " ch))
  (loop []
    (let [work  @exec
          tasks (-> work :sv-index (get ch))]
      (when (seq tasks)
        (let [reindexed (-> (reduce (reindex-task ch val) work tasks)
                            (update :sv-index dissoc ch)
                            (update :sv-check assoc ch val))]
          (when-not (tx! exec work reindexed)
            (recur)))))))

(defn send! [ctx key val]
  (let [exec (get ctx :exec)
        ch   (get ctx key)]
    ;; (println "sending" key ch val)
    (cond
      ;; interface with clj
      (fn? ch)  (ch val)
      (nil? ch) (let [e (ast/xkey :error)]
                  (if (contains? ctx e)
                    (send! ctx e ["msg on unbound channel" [key val]])
                    (throw (RuntimeException. (str "msg on unbound channel: "
                                                   key " <- " val)))))
      true      (deliver! exec ch val))))

(defn index-task [index {:keys [waiting] :as task}]
  (reduce (fn [index sv] (update index sv (fnil conj #{}) task)) index waiting))

(defn enqueue [exec task]
  ;; (println "queuing" (dissoc task :ctx))
  (let [wait-points (svs task)]
    (if (empty? wait-points)
      (txf! exec #(update % :stack conj task))
      (let [task (assoc task :waiting wait-points)]
        (txf! exec #(update % :sv-index index-task task))))))

;; Adds tasks one at a time. It would be more efficient to batch, but it
;; complicates the code horribly.
(defn net {:style/indent 1} [ctx & tasks]
  (let [exec (get ctx :exec)]
    (loop [tasks tasks]
      (when (seq tasks)
        (if (enqueue exec (update (first tasks) :ctx #(merge ctx %)))
          (recur (rest tasks))
          (recur tasks))))))

(defn run! [task]
  ((:call task) (:ctx task) (:args task)))

(defn next-task [exec]
  (let [work  @exec
        ready (:stack work)]
    ;; (println ">-->:" (map #(dissoc % :ctx) ready))
    (when (seq ready)
      (if (tx! exec work (assoc work :stack (pop ready)))
        (peek ready)
        (recur exec)))))

(defn drain [executor]
  (loop [task (next-task executor)]
    (when task
      (run! task)
      (recur (next-task executor))))
  ::halt)

(defn return {:style/indent 1} [ctx val]
  (send! ctx (ast/xkey :return) val))

(defn emit! [ctx [state kvs]]
  ;; (println "!!" state kvs)
  (if (or (:μ? state) (some #(not (ast/keyword? (first %))) kvs))
    (return ctx (ast/emission kvs))
    (loop [[kv & kvs] kvs]
      (when kv
        (if (contains? ctx (first kv))
          (send! ctx (first kv) (second kv))
          ;; TODO: "unbound" channel
          (send! ctx (ast/xkey :error) (str "message to unbound channel: " kv (map type kv))))
        (recur kvs)))))

(defn resolve [ctx [state form]]
  (return ctx
    (cond
      ;; Inputs are going to be treated differently now
      (:inhibit? state)    (ast/immediate form)
      (env/captured? state (ast/symbol form))
      (ast/input (ast/symbol form) (env/capid state (ast/symbol form)))
      (ast/ref? form)      (:binding form)
      (ast/symbolic? form) (throw (RuntimeException. (str "unbound symbol: " form)))
      ;; (ast/immediate form) ; ^|^
      true                 (assert false "unreachable!!"))))

(defn apply-μ [ctx [state μ arg]]
  (println "applying " μ arg))

(defn call-extern [ctx [state f tail]]
  ;; Invoke externs directly. How do we compile this exactly? Maybe I should use
  ;; names so as to indirect and intercept these calls?
  ((:fn f) ctx state f tail))

(declare walk)

(defn apply [ctx [state head tail]]
  (cond
    (ast/μ? head)        (let [v (sv)]
                             (net ctx
                               {:call walk
                                :ctx  {ret v}
                                :args [state tail]}
                               {:call apply-μ
                                :args [state head v]}))
    (ast/external? head) (net ctx {:call    call-extern
                                   :waiting (svs tail)
                                   :args    [state head tail]})
    ;; (ast/incomplete? head) (let [[ch st] (pipe 1)]
    ;;                          (net ctx
    ;;                            {:call walk
    ;;                             :ctx  {ret ch}
    ;;                             :args [state tail]}
    ;;                            {:call (fn [ctx [h t]]
    ;;                                     (return ctx (ast/application h t)))
    ;;                             :args [head (first st)]}))

    true (throw (RuntimeException. (str head " is not applicable!")))))

(defn eval [ctx [state form]]
  (cond
    (ast/coll? form) (net ctx
                       {:call walk
                        :args [state (into (empty form) (map ast/immediate) form)]})
    (ast/pair? form) (let [sync (sv)]
                       (net ctx
                         {:call eval
                          :ctx  {ret sync}
                          :args [state (:head form)]}
                         {:call apply
                          :args [state sync (:tail form)]}))

    (ast/symbolic? form)   (net ctx {:call resolve :args [state form]})
    ;; (ast/incomplete? form) (return ctx (ast/immediate form))
    true                   (return ctx form)))

(defn walk [ctx [state form]]
  (cond
    (ast/immediate? form)   (net ctx {:call eval
                                      :ctx  ctx
                                      :args [state (:form form)]})
    (ast/application? form) (let [sync (sv)]
                              (net ctx
                                {:call walk
                                 :ctx  {ret sync}
                                 :args [state (:head form)]}
                                {:call apply
                                 :args [state sync (:tail form)]}))
    (ast/pair? form)        (let [sync1 (sv)
                                  sync2 (sv)
                                  state (-> state (assoc :μ? true) (assoc :inhibit? true))]
                              (net ctx
                                {:ctx  {ret sync1}
                                 :call walk
                                 :args [state (:head form)]}
                                {:ctx  {ret sync2}
                                 :call walk
                                 :args [state (:tail form)]}
                                {:call (fn [ctx [h t]] (return ctx (ast/pair h t)))
                                 :args [sync1 sync2]}))
    (ast/symbolic? form)    (let [sym (ast/symbol form)]
                              (return ctx (if (env/captured? state sym)
                                            (ast/input sym (env/capid state sym))
                                            form)))
    (ast/coll? form)        (let [syncs (take (count form) (repeatedly sv))]
                              (clojure.core/apply
                               net ctx
                               {:call (fn [ctx coll] (return ctx (into (empty form) coll)))
                                :args syncs}
                               (map (fn [f sync]
                                      {:ctx  {ret sync}
                                       :call walk
                                       :args [state f]})
                                    form syncs)))
    (ast/μ? form)           (let [s (-> state (env/uncapture (:param form)) (assoc :μ? true))

                                  sync (sv)]
                              (net ctx
                                {:call walk
                                 :ctx  {ret sync}
                                 :args [s (:body form)]}
                                {:call (fn [ctx [body]]
                                         (return ctx (assoc form :body body)))
                                 :args [sync]}))
    (ast/emission? form) (let [sync (sv)]
                           (net ctx
                             {:call walk
                              :ctx  {ret sync}
                              :args [state (:kvs form)]}
                             {:call emit!
                              :args [state sync]}))
    true                 (return ctx form)))


(defn entry [form ctx]
  (let [exec (executor)]
    (net (assoc ctx :exec exec)
      {:call walk
       :args [{} form]})
    (drain exec)))

(defn try-call! [f]
  (fn [ctx [args]]
    (if (ast/incomplete? args)
      ;; REVIEW: We should be more careful about returning incomplete
      ;; calculations. An incomplete calculation should always depend on
      ;; something. If it's ready but uncompleteable, that should trigger an
      ;; error.
      (throw (RuntimeException. "not implemented."))
      (return ctx (clojure.core/apply f args)))))

(defn primitive [name f]
  (ast/extern name (fn [ctx state head tail]
                     ;; (println "extern" name)
                     (let [sync (sv)]
                       (net ctx
                         {:call walk
                          :ctx {ret sync}
                          :args [state tail]}
                         {:call (try-call! f)
                          :args [sync]})))))

(defn primitives [m]
  (reduce (fn [acc [k v]] (assoc acc (ast/symbol k) (primitive k v))) {} m))

(def fns
  (primitives
   {"+*"   +
    "**"   *
    "-*"   -
    "/*"   /
    ">*"   >
    "<*"   <
    "=*"   =
    "mod*" mod
    "not*" (fn [x] (assert (boolean? x)) (not x))
    "str*" str

    "list?*"   ast/list?
    "map?*"    ast/map?
    "dot?*"    ast/dot?
    "string?*" string?

    "merge*"  merge
    "get*"    get

    "symbol?*" ast/symbolic?}))


(defn createμ [ctx [id param exec body]]
  (println body)
  )

(def tee (executor))

(def base-env
  (merge
   fns
   ;; Structural manipulations. Come to think of it, these might need to be a
   ;; different class.
   {(ast/symbol "nth*")    (ast/extern "nth*" (fn [ctx _ _ [v i]]
                                                (assert (and (vector? v) (integer? i)))
                                                (return ctx (nth v (dec i)))))
    (ast/symbol "first*")  (ast/extern "first*" (fn [ctx _ _ v]
                                                  (assert (ast/coll? v))
                                                  (return ctx (first v))))
    (ast/symbol "rest*")   (ast/extern "rest*" (fn [ctx _ _ v]
                                                 (assert (ast/coll? v))
                                                 (return ctx (into [] (rest v)))))
    (ast/symbol "count*")  (ast/extern "count*" (fn [ctx _ _ v]
                                                  (assert (ast/coll? v))
                                                  (return ctx (count v))))
    (ast/symbol "empty?*") (ast/extern "empty?*" (fn [ctx _ _ v]
                                                   (assert (ast/coll? v))
                                                   (return ctx (boolean (empty? v)))))
    ;; The language itself
    (ast/symbol "emit")    (ast/extern "emit" (fn [ctx state _ tail]
                                                (println state tail)
                                                (let [sync (sv)]
                                                  (net ctx
                                                    {:call walk
                                                     :ctx  {ret sync}
                                                     :args [state tail]}
                                                    {:call emit!
                                                     :args [state sync]}))))
    (ast/symbol "μ")       (ast/extern "μ" (fn [ctx s _ [param body]]
                                             (let [id   (sv)
                                                   s'   (-> s (assoc :μ? true)
                                                                 (env/capture param id))
                                                   exec (executor)
                                                   sync (sv)]
                                               (net ctx
                                                 {:call walk
                                                  :ctx  {ret   sync
                                                         :exec tee}
                                                  :args [s' body]}
                                                 {:call createμ
                                                  :args [id param exec sync]}))))}))
