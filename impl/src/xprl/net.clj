(ns xprl.net
  (:refer-clojure :exclude [resolve eval apply delay])
  (:require [xprl.ast :as ast]
            [xprl.env :as env]
            [xprl.system :as sys]))

(defn stream-value [key n]
  {:node :stream-val
   :key  key
   :n    n})

(defn pipe [& [n]]
  (let [key (gensym "pipe")
        ch  {:node :channel
             :key  key}]
    (if (nil? n)
      [ch (map (fn [i] (stream-value key i)) (map inc (range)))]
      (do
        (assert (= 1 n) "pipes of fixed length > 1 not supported")
        [(assoc ch :single? true) [(stream-value key 1)]]))))

(defn stream-value? [x]
  (= :stream-val (:node x)))

;; N.B.: These work queues are intended to be thread local.
(def work (atom {:ready [] :waiting [] :index {} :streams {} :gen 0}))

(defn waitings [task]
  (into #{} (filter stream-value?) (:args task)))

(defn tx! {:style/indent 1} [old new]
  (compare-and-set! work old (assoc new :gen (inc (:gen old)))))

(defn send! [ch val]
  (let [{:keys [ready waiting index streams] :as w} @work

        n       (inc (count (get streams (:key ch))))
        tag     (stream-value (:key ch) n)
        waits   (get-in index [(:key ch) n])
        toready (into [] (filter #(= 1 (count (:waiting %))) waits))]
    ;; REVIEW: Why keep waits in a list? The index should be enough by itself
    (reduce (fn [w task]
              (if ())))
    ))

(defn index-tasks [w ready wait index]
  (let [s        (:streams w)
        realised (filter (fn [[{:keys [key n]} _]]
                           (and (contains? s key) (<= n (count (get s key)))))
                         index)]
    (if (empty? realised)
      ;; easy path
      (-> w
          (update :ready into ready)
          (update :waiting into wait)
          (update :index #(reduce (fn [acc [sv t]]
                                    (update acc (:key sv) update (:n sv) (fnil conj []) t)))))
      (throw (RuntimeException. "not implemented")))))

(defn net [tasks]
  (let [ts    (into [] (map (fn [x] (assoc x :waiting (waitings x)))) tasks)
        ready (into [] (comp (filter #(empty? (:waiting %)))) ts)
        wait  (into [] (comp (remove #(empty? (:waiting %)))) ts)
        index (mapcat (fn [x] (map (fn [w] [w x]) (:waiting x))) ts)]
    (loop [w @work]
      (when-not (tx! w (index-tasks w ready wait index))
        (recur @work)))))


(defn exec [task ctx]
  (case (:node task)
    :task ((:call task) ctx (:args task))))

(defn next-task []
  (let [w @work
        r (:ready w)]
    (when (seq r)
      (if (tx! w (update w :ready pop))
        (peek r)
        (recur)))))

(defn drain [top-ctx]
  (loop [task (next-task)]
    (when task
      (exec task (merge top-ctx (:ctx task)))
      (recur (next-task))))
  ;; REVIEW: When we've processed everything that can be processed just now,
  ;; what do we do with the remainder? Should there be a remainder, or should
  ;; the remaining computation have been sent somewhere already?
  ::halt)

(defn return [ctx val]
  (send! (:return ctx) val))

(defn apply [ctx [state head tail]])

(defn eval [ctx [state form]])

(defn walk [ctx [state form]]
  (cond
    (ast/immediate? form)   (eval ctx state (:form form))
    (ast/application? form) (let [[ch st] (pipe 1)]
                              (net [{:node :task
                                     :call walk
                                     :ctx  {:return ch}
                                     :args [state (:head form)]}
                                    {:node :task
                                     :call apply
                                     :args [state (first st) (:tail form)]}]))
    (ast/pair? form)        (let [[ch1 st1] (pipe 1)
                                  [ch2 st2] (pipe 1)

                                  s (-> state (assoc :μ? true) (assoc :inhibit? true))]
                              (net [{:node :task
                                     :ctx  {:return ch1}
                                     :call walk
                                     :args [s (:head form)]}
                                    {:node :task
                                     :ctx  {:return ch2}
                                     :call walk
                                     :args [s (:tail form)]}
                                    {:node :task
                                     :call #(ast/pair (first %) (second %))
                                     :args [(first st1) (first st2)]}]))
    (ast/symbolic? form) (let [sym (ast/symbol form)]
                           (return ctx (if (env/captured? state sym)
                                         (ast/input {} sym (env/capid state sym))
                                         form)))
    (ast/coll? form)     (let [ps (take (count form) (repeatedly #(pipe 1)))]
                           (net (conj (map (fn [f [ch _]]
                                             {:node :task
                                              :ctx  {:return ch}
                                              :call walk
                                              :args [state f]})
                                           form ps)
                                      {:node :task
                                       :call #(into (empty form) %)
                                       :args (into [] (map (comp first second)) ps)})))
    ;; TODO: μ
    ;; Emission
    ;; call?
    true                 (return ctx form)))


(defn entry [form ctx]
  (net [{:node :task
         :ctx  {}
         :call walk
         :args [{} form]}])
  (drain ctx))
