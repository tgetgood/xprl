(ns xprl.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [xprl.ast :as ast]
            [xprl.tag-fns :as tag]))

(defn tag []
  (gensym "%"))

(def empty-env {:captured {}})

(defn capture [env s id]
  (assoc-in env [:captured s] id))


;; (defn apply
;;   ([form env] (apply (:head form) (:tail form) env))
;;   ([head tail env]
;;    (cond
;;      (ast/external? head) (head form env)
;;      (ast/μ? head)        (compile-μ-apply head tail env)
;;      :else                (throw (Exception. (str "bad application:" head "," tail))))))

;; (defn eval [form env]
;;   (cond
;;     (ast/symbolic? form) (resolve form env)
;;     (ast/pair? form)     (compile (ast/immediate (:head form)) env
;;                                   {:call :apply
;;                                    :args (:tail env)
;;                                    :env  env})))

;;;;; The following can all be memoised if they start to take up appreciable
;;;;; amounts of time.

(defn tag? [x] ; FIXME: Not the most robust test. But simple.
  (and (symbol? x) (str/starts-with? (name x) "%")))

(defn tagset [x]
  (when (ast/call? x)
    (into #{} (filter tag?) (ast/call-vars x))))

(defn sort-by-deps
  "Returns a map from tags to sets of tags. This map represents the 'depends
  upon' relation, i.e. expresses data dependencies."
  [routine]
  (reduce (fn [deps [tag val]]
            (let [refs (tagset val)] ; just like the old `symbols` code.
              (if (seq refs) ; don't add empty sets.
                (update deps tag set/union refs)
                deps)))
          {} routine))

(defn value? [x]
  (not (ast/call? x)))

(defn trace-values
  "Returns a map from tags to values. If a tag is not in the map, it does not
  have a known value. (That doesn't mean it won't eventually, but we can't do
  anything with it as yet). "
  [routine deps]
  (let [edge (set/difference (set (keys routine)) (set (keys deps)))]
    (into {} (comp (filter #(value? (get routine %)))
                   (map (fn [t] [t (get routine t)])))
          edge)))

(defn runnable? [vmap [t inst]]
  (every? #(contains? vmap %) (tagset inst)))

(defn lowest-runnable
  "Returns the \"first\" (i.e. closest to the edge of the dependency graph)
  instruction which is ready to execute (all args have values). There will in
  general be multiple candidates, which one is chosen is arbitrary and
  susceptible to change."
  [routine valmap]
  (first (filter (partial runnable? valmap) routine)))

(defn extract-args
  "Returns the args of `inst` from `vmap` in order. Assumes that the args have
  values and triggers an error if not."
  [inst vmap]
  (let [args (into [] (comp (filter tag?) (map (partial get vmap))) (ast/call-vars inst))]
    (assert (not-any? nil? args))
    args))

(defn prune
  "Removes all nodes of routine graph on which `:return` does not (transitively)
  depend."
  [routine]
  ;; TODO: This will work, but we really want to keep the graph size down.
  routine)

;;;;; And the heart of the matter

(defn init [form]
  (let [t (tag)
        e (tag)]
    {:return (ast/call :compile e t)
     t       form
     e       empty-env}))

;; This method of compiling makes some progress each time you invoke it until it
;; doesn't. Once we reach a fixed point, that's as far as we get until we have
;; more information (either compiling into another unit, or calling at runtime.
;; The distinction is somewhat fuzzy).
(defn compile-step [rout]
  (let [deps       (sort-by-deps rout)
        vmap       (trace-values rout deps)
        [tag call] (lowest-runnable rout vmap)
        inst       (:inst call)
        args       (extract-args call vmap)
        expansion  (clojure.core/apply (tag/fns inst) args)]
    (prune (merge rout (set/rename-keys expansion {:return tag})))))
