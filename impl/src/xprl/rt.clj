(ns xprl.rt
  (:require [xprl.ast :as ast]))

;;;;; Preamble

(defonce the-system (atom nil))
(defonce ^:dynamic *the-executor* nil)

;; REVIEW: Should each executor have a different root task?
;; The root task isn't a real task, just a marker for the executor loop itself.
;; There are N loops, so should there be N root task ids?
;; Well, the executor tasks never complete, and can't have complete handlers, so
;; does it really matter?
;; Plus with work stealing, we'd have to fiddle with the tree and reroot it
;; otherwise the index would get invalidated.
(defonce root-task (gensym "root-task-"))
(defonce ^:dynamic *current-task* root-task)

;;;;; Continuation Manipulation

(def ret (ast/xkey :return))

(declare return)

(defn with-return [env retfn]
  (assoc env ret retfn))

(defn ret-> {:style/indent [1]} [env inner outer]
  (inner (with-return env outer)))

(defn error! [env msg]
  ((get env (ast/xkey :error)) msg))

;;;;; Cables
;;
;; `cable` is just the term I'm using for "bundle of continuations".
;;;;;

(defn cut "Create a 'spliced cable' from given cable."
  [cable id]
  {::cut? true ::id id})

(defn cut? [cable]
  (::cut? cable))

(defn captured? [cable]
  (::captured? cable))

;;;;; Tasks
;;
;; A tasks is a rather clunky thing: it's a pair consisting of a bundle of
;; callbacks and a function that takes that bundle, does something, and then
;; calls zero or more of these callbacks zero or more times each.
;;
;; I suppose I could say calls all of these callbacks zero or more times each...
;;
;; The reason it's not just a thunk is that we need to be able to modify the
;; callback bundle to override behaviour; catch outputs before sending them on,
;; spoof inputs, etc..
;;
;; This is the only way I can figure to make networks do what they ought. It's
;; also going to be critical to the nested sandbox approach to security at the
;; heart of the language.
;;
;; Using Koestler's terms, the function is the holon, the unit which is
;; autonomous from within; given input it does what it does, and the callback
;; bundle is its only connection to the outside world. Whoever can manipulate
;; that callback bundle can view the task as subordinate by controlling what
;; goes in and out, but it can't change what it does.
;;
;; I'm digressing. This belongs in the docs.
;;;;;

;; TODO: If none of the ancestors of `task` have completion handlers, then
;; there's no point indexing a task.
;; REVIEW: What could go wrong with not keeping tabs on a task?
(defn index-task! [{:keys [parent id] :as task}]
  (swap! *the-executor*
         update :index
         #(-> %
              (update-in [parent :children] (fnil conj #{}) id)
              (assoc id (assoc task :children #{})))))

(defn task {:style/indent [1]}
  ([env f] (task env f nil))
  ([env f on-complete]
   (let [m (merge
            {:parent *current-task*
             :id     (gensym "task-")
             :env    env}
            (when on-complete
              ;; N.B.: This indexes the completion handler before the the task it
              ;; waits on is even created, but that's fine because it won't be
              ;; ~enqueued~ until after the waited-upon task is finished (which
              ;; had better be *after* it has been created).
              {:on-complete (task env on-complete)}))]
     (index-task! m)
     (with-meta f (merge (meta f) m)))))

(defn enqueue!
  ([task]
   (enqueue! *the-executor* task))
  ([exec task]
   (swap! exec update :work conj task)
   nil))

(defn enqueue-all!
  ([tasks] (enqueue-all! *the-executor* tasks))
  ([exec tasks] (swap! exec update :work #(into % tasks))))

(defn clear-finished [index id]
  (let [{:keys [parent children on-complete]} (get index id)]
    (assert (not (nil? parent)) (str id " has no parent:" (get index id)))
    (cond
      (= id root-task)  index
      (empty? children) (let [i' (-> index
                                     (dissoc id)
                                     (update-in [parent :children] disj id)
                                     (clear-finished parent))]
                          (with-meta i' (update (meta i') :cbs conj on-complete)))
      true              index)))

(defn deindex-task! [task]
  (let [exec   @*the-executor*
        index  (:index exec)
        index' (clear-finished index (:id (meta task)))]
    (if (compare-and-set! *the-executor* exec (assoc exec :index (with-meta index' {})))
      ;; This has to be here since we DO NOT want to enqueue completion
      ;; callbacks more than once.
      (when-let [completions (remove nil? (:cbs (meta index')))]
        (when (seq completions)
          (enqueue-all! completions)))
      ;; spin!
      (recur task))))

(defn run-task [t]
  (binding [*current-task* (:id (meta t))]
    (cond
      (fn? t) (let [{:keys [env]} (meta t)]
                   (t (with-return env
                           (fn [x]
                             ;; REVIEW: Special baked-in behaviour of nets.
                             (if (ast/net? x)
                               (enqueue-all!
                                (map (fn [f]
                                       (task env #((:walkfn x) % (ast/immediate f))))
                                     (:forms x)))
                               (return env x))))))
      true       (throw (RuntimeException. (str "Bad task type " (type t) ": " t))))
    (deindex-task! t)))

;;;;; Wires
;;
;; Wires — widely refered to as `pipes` in older docs — are akin to channels in
;; go or core.async.
;;
;; The difference is that they have two distinct refereces, one write only, and
;; one read only. The write only reference is a black hole: messages go in, you
;; know they've gone in, and that's that.
;;
;; The read only side is where it's interesting. It's an immutable, lazy,
;; potentially infinite list whose elements will eventually consist of those
;; values passed into the write end.
;;
;; Attempts to read values that aren't there yet park until the relevant values
;; are available.
;;
;; N.B.: if you retain the head of this list, you've effectively created a
;; channel with an infinite buffer. This is probably a mistake. I should do
;; something to warn you.
;;
;; Also note that writes to channels which nobody has tried to read will also
;; park, thus creating backpressure.
;; (FIXME: That last bit isn't implemented yet).
;;;;;

(defn wire [& init]
  (let [w (ast/wire)]
    (when (seq init)
      (swap! (:state w) assoc :stream (vec init)))
    w))

;; FIXME: I'm far from convinced these are threadsafe. delivery should be
;; possible from multiple executors and reads should be as if it were immutable.
;; But of course it isn't under the hood and that complicates things so much...

(defn next-wire [w]
  (update w :offset inc))

(defn try-read! [env w]
  (let [state  @(:state w)
        offset (- (:offset w) (:offset state))]
    (assert (not (neg? offset)) "Trying to read freed stream segment!")
    (if (< offset (count (:stream state)))
      ;; if we have a value, return it
      (return env (nth (:stream state) offset))
      ;; otherwise park and wait
      (let [w' (update state :listeners update (:offset w) (fnil conj []) env)]
        (if (compare-and-set! (:state w) state w')
          nil
          ;; ::parked
          ;; spin!
          ;; REVIEW: I need these spinning cas ops for correctness, which
          ;; probably means atoms are the wrong primitive.
          (recur env w))))))

(defn drain-listeners! [wire offset value]
  (let [envs (get (:listeners @(:state wire)) offset)]
    (when (seq envs)
      (swap! (:state wire) update :listeners dissoc offset)
      (enqueue-all! (map (fn [e] (task e (fn [e] (return e value)))) envs))
      nil)))

(defn deliver! [wire v]
  (let [state @(:state wire)
        next  (update state :stream conj v)]
    (if (compare-and-set! (:state wire) state next)
      (drain-listeners! wire (dec (+ (:offset next) (count (:stream next)))) v)
      (recur wire v))))

;;;;; Emission

(defn send-captured! [env msgs]
  (let [ch (::capture-chan env)]
    (run! ch msgs)))

(defn send-1! [env [k v]]
  (if (contains? env k)
    (let [ch (get env k)]
      (cond
        (ast/wire? ch) (deliver! ch v)
        (fn? ch)       (ch v)
        true           (throw (RuntimeException.
                           (str "Bad channel type: " (type ch) " " ch))))
      nil) ; prevent return value from being used.
    (do
      ;; TODO: :unbound channel
      ;; TODO: Keep errors in xprl.
      (throw (RuntimeException. (str "Cannot send " v " to " k ". No such channel."))))))

(defn return [env x]
  (assert (not (nil? x)) "nil cannot be sent as a message. It is not a value in xprl.")
  (send-1! env [ret x]))

(defn send! [env msgs]
  (run! (partial send-1! env) msgs))

(defn do-emission! [env msgs]
  (cond
    (cut? env)      (return env (ast/emission msgs))
    (captured? env) (send-captured! env msgs)
    true            (send! env msgs)))

;;;;; Executors
;;
;; An executor is a dedicated thread which runs an event loop.
;; There will eventually be one executor per core (physical or logical? I don't
;; know yet) and they will steal work from each other to stay active.
;; Most of this is lifted straight from cilk and adapted for immutability (which
;; actually makes what cilk did a lot easier).

(defn empty-executor! []
  (atom {:work  []
         :index {root-task {:id root-task :parent ::none}}}))

(defn run [exec]
  (binding [*the-executor* exec]
    (println "starting executor")
    (loop []
      (try
        (let [ems (:work @exec)]
          (if (seq ems)
            ;; TODO: dosync for work stealing.
            (let [task (peek ems)]
              ;; Remove task from work stack *before* running it!
              (swap! exec update :work pop)
              ;; This should block the thread until it goes to sleep
              (run-task task))
            (Thread/sleep 500)))
        (catch Throwable e
          (binding [*out* *err*]
            (println e))))
      ;; At which point we find something else to do
      (recur))))

;;;;; System
;;
;; The `system` is what I'm calling the thing that starts and coordinates executors.
;; It will eventually be responsible for coordination between machines.
;; Notably it is *not* the "system" that wraps syscalls, I need a different name
;; for one of these.
;;
;; I guess I could just call this "the runtime"... But it's only part of the
;; runtime, isn't it?
;;;;;

;; The clj repl interacts with the executor threads by injecting work into the
;; queue of one of theme. Which? it doesn't matter, in principle. Just use the
;; first for now.
(defn seed! [env taskfn]
  (let [exec (first (:executors @the-system))]
    (binding [*the-executor* exec]
      (enqueue! (task env taskfn))))
  nil)

(defn start-executors! []
  ;; Each executor owns a thread.
  (run! #(.start (Thread. (fn [] (run %)))) (:executors @the-system)))

(defn populate-executors! []
  ;; TODO: One per core? (.availableProcessors (Runtime/getRuntime))
  ;; Maybe just one per physical CPU, but that will need testing.
  ;; But even if it's better to ignore hyperthreads, there's no portable way to
  ;; figure this out anyway (as far as I know). So, more runtime optimisations...
  ;; FIXME: This is set to one since there's not point in more until work
  ;; stealing is implemented.
  [(empty-executor!)])

(defn init! []
  (when (compare-and-set! the-system nil {:executors (populate-executors!)})
    (start-executors!)))
