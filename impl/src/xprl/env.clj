(ns xprl.env
  (:refer-clojure :exclude [bound? resolve])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

(defn strip
  "Removes lexical env from a form"
  [x]
  (dissoc x ::lex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-ns {})

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/unresolved? (keys ns)) ns)
  (walk/postwalk #(if (contains? ns %) (get ns %) %) body))

(defn ns-intern [ns sym val]
  (assert (ast/unresolved? sym) sym)
  (assoc ns (strip sym) val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

(defn ns-resolved? [sym env]
  (and (ast/resolved? sym)
       (not (contains? (:unresolve env) (strip (ast/sym sym))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Lexical env in AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dyn->lex [env]
  (merge env
         {:bindings (into {} (comp (map (fn [[k v]] (when-not (empty? v)
                                                      [k (peek v)])))
                                   (remove nil?))
                          (:bindings env))
          :blocks   (:blocks env)}))

(defn merge-env [outer inner]
  (if (nil? inner)
    outer
    (merge outer
           {:bindings (merge (reduce dissoc (:bindings outer) (:blocks inner))
                             (:bindings inner))
            :blocks   (into (or (:blocks outer) #{})
                            (remove #(contains? (:bindings outer) %))
                            (:blocks inner))})))

(defn attach
  "Merges `env` into the lexical env of `form` if any and updates `form` with
  the new env."
  [form env]
  (trace! "incorporating" (::lex form) "into" env)
  (let [env (merge-env env (::lex form))]
    (trace! "->" env)
    (cond
      (empty? env)   form
      (vector? form) (mapv #(attach % env) form)

      ;; This is a mess. We don't want keywords or other datatypes modified with
      ;; useless envs.
      (or (ast/pair? form) (ast/application? form) (ast/immediate? form)
          (ast/emission? form) (ast/symbol? form) (ast/μ? form))
      (assoc form ::lex env)

      (record? form) form
      (map? form)    (into {} (map (fn [[k v]] [(attach k env) (attach v env)]) form))
      true           form)))

(defn attach-dyn
  "Like `attach` but first lexicalises the dynamic env `env`."
  [form env]
  (attach form (dyn->lex env)))

(defn bindμ [{:keys [name params body] :as μ} args]
  (trace! "binding μ:" (merge {params args} (when name {name μ})))
  (attach body {:bindings (merge {params args} (when name {name μ}))}))

(defn block [form & syms]
  (trace! "blocking:" syms)
  (attach form {:blocks (set (map strip syms))}))

(defn capture [args]
  (let [names (mapv strip (map ast/sym (butlast args)))]
    (when (every? ast/unresolved? names)
      (conj names (last args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Dynamic env During Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-env ::root)

(defn popbind [m k]
  (update m (strip k) #(if (empty? %) % (pop %))))

(defn pushbind [m [k v]]
  (trace! "pushing binding" k "=" v)
  (update m (strip k) conj v))

(defn walk-μ [env {:keys [name params]}]
  (-> env
      (assoc :μ? true)
      ;; Here's a tricky one. We don't want any bindings for `name` or `params`
      ;; to leak through when walking the body of a μ: they must remain
      ;; unbound. So while walking we remove those bindings completely and rely
      ;; on them being reinserted on any future traversal where args are applied
      ;; to the μ.
      (update :bindings dissoc name params)
      (update :unresolve (set (remove nil? [name params])))))

(defn walk-channels [env {:keys [chs]}]
  (update env :ctx merge chs))

(defn incorporate [form env]
  (if-let [local (::lex form)]
    (update env :bindings #(as-> % bindings
                             (reduce popbind bindings (:blocks local))
                             (reduce pushbind bindings (:bindings local))))
    env))

(defn bound? [sym env]
  (let [{:keys [bindings]} (incorporate sym env)
        s                  (strip (ast/sym sym))]
    (when (contains? bindings s)
      (not (empty? (get bindings s))))))

(defn resolve [sym env]
  (let [{:keys [bindings]} (incorporate sym env)
        s                  (strip (ast/sym sym))
        res                (peek (get bindings s))]
    (block res s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Frames Rewrite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-env ::root)
(def μ-env {::μ? true})

(defn μ-ctx? [env]
  (cond
    (= ::root env) false
    (::μ? env)     true
    true           (recur (::previous env))))

(defn push [env frame]
  (assoc frame ::previous env))

;; Not reverse! something much weirder.
(defn invert [s]
  (if (= (::previous s) ::root)
    s
    (recur (assoc (::previous s) ::next s))))

(defn merge-stacks
  ;; AKA despaghettify
  "Given two stacks, find their common root and create a new stack of the form
  root<-unique part of `outer`<-unique part of `inner`."
  [inner outer]
  (if (= inner outer) ; this case would lead to infinite looping below
    inner
    (loop [i (invert inner)
           o (invert outer)]
      (if (= (::previous (::next i)) (::previous (::next o)))
        (recur (::next i) (::next o))
        (loop [root (::previous i)
               o    o]
          (if (contains? o ::next)
            (recur (push root (dissoc o ::next)) (::next o))
            (loop [root (push root o)
                   i    i]
              (if (contains? i ::next)
                (recur (push root (dissoc i ::next)) (::next i))
                (push root i)))))))))

(defn attach [env form]
  (cond
    (vector? form) (into [] (map (partial attach env)) form)
    (map? form)    (assoc form ::env env)
    true           form))

(defn resolve [env {:keys [sym] :as im}]
  (let [s (strip (ast/sym sym))]
    (loop [{:keys [bindings] :as env} env]
      (cond
        (= ::root env)         im
        (contains? bindings s) (let [next (get bindings s)] ; `next` might be `false`!
                                 (attach next (merge-stacks (::env next) (::previous env))))
        true                   (recur (::previous env))))))
