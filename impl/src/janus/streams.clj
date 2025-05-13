(ns janus.streams)


(defn channel []
  (atom {:pending-writes (clojure.lang.PersistentQueue/EMPTY)
         :pending-reads  (sorted-map)
         :offset         0
         :subscriptions  {}}))

;; The cache can be extended but never shortened.
(defn stream [cached-els offset ch]
  (let [name (gensym)
        st   {:cache  (atom (into [] cached-els))
              :name   name
              :offset offset
              :ch     ch}]
    (swap! ch update :subscriptions assoc name st)
    st))

(defn extend-cache [st val]
  (swap! (:cache st) conj val))

(defn destroy-stream [st]
  (swap! (:ch st) :update :subscriptions :dissoc (:name st)))

(defn deliver! [{:keys [msg cb]} offset reads subscriptions]
  ;; REVIEW: I'm using loops to emphasize the imperative nature of this
  ;; mechanism. It's pretty ugly, but maybe it should be...
  (event! :deliver! {:msg msg})
  (loop [subs subscriptions]
    (when (seq subs)
      (let [[k v] (first subs)]
        (extend-cache v msg)
        (recur (rest subs)))))
  (when (= offset (key reads))
    (loop [reads (val reads)]
      (when (seq reads)
        ((first reads) msg)
        (recur (rest reads)))))
  (cb))

(defn flush! [ch]
  (let [state    @ch
        write    (peek (:pending-writes state))
        read     (first (:pending-reads state))
        ;; It's possible no one wants the next value, but someone is waiting for
        ;; a value farther down the line.
        deliver? (= (key read) (:offset state))
        post     (-> state
                     (update :pending-writes pop)
                     (update :offset inc)
                     (update :pending-reads #(if deliver?
                                               (dissoc % (key read))
                                               %)))]
    (when (and write read)
      (when (compare-and-set! ch state post)
        (deliver! write (:offset state) read (:subscriptions state)))
      (recur ch))))

(defn ch-next [ch offset cb]
  (swap! ch update :pending-reads update offset conj cb)
  (flush! ch))

(defn send! [ch msg cb]
  (event! :send!) {:msg msg}
  (swap! ch update :pending-writes conj {:msg msg :cb cb})
  (flush! ch))

(defn stream-first [st ccs]
  (if-let [v (first @(:cache st))]
    (return ccs v)
    (ch-next (:ch st) (:offset st) (fn [msg] (return ccs msg)))))

(defn stream-rest [st ccs]
  (return ccs (stream (rest @(:cache st)) (inc (:offset st)) (:ch st))))

;; Should return a pair of (channel, stream)
(defn chan []
  (let [ch (channel)]
    [ch (stream [] 0 ch)]))

;; special channel whose stream will only ever be called with `(last st)`, so we
;; can skip a bunch of logic and ensure that writes always happen immediately
;; and thus we can ensure read-after-write semantics.
(defn last-ch [])
