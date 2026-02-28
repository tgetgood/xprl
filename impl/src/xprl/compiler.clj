(ns xprl.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [xprl.ast :as ast]
            [xprl.tag-fns :as tag]))

(defn tag []
  (gensym "%"))

;;;;; The following can all be memoised if they start to take up appreciable
;;;;; amounts of time.

(defn tag? [x] ; FIXME: Not the most robust test. But simple.
  (and (symbol? x) (str/starts-with? (name x) "%")))

(defn tagset [x]
  #_(when (ast/call? x)
    (into #{} (filter tag?) (:args x))))

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
  #_(not (or (ast/call? x) (ast/input? x))))

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
  (and #_(ast/call? inst)
       (every? #(contains? vmap %) (:args inst))))

(defn lowest-runnable
  "Returns the \"first\" (i.e. closest to the edge of the dependency graph)
  instruction which is ready to execute (all args have values). There will in
  general be multiple candidates, which one is chosen is arbitrary and
  susceptible to change."
  [routine valmap]
  (first (filter (partial runnable? valmap) routine)))

(defn prune
  "Removes all nodes of routine graph on which `:return` does not (transitively)
  depend."
  [routine]
  ;; TODO: This will work, but we really want to keep the graph size down.
  routine)

;;;;; And the heart of the matter

(defn compile [form])
