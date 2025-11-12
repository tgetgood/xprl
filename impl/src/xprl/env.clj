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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Env Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-env ::root)

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
      (if (= (dissoc i ::next) (dissoc o ::next))
        (recur (::next i) (::next o))
        (cond ; the first two cases are just an optimisation.
          (nil? i) outer
          (nil? o) inner
          true
          (loop [root (::previous o)
                 o    o]
            (if (contains? o ::next)
              (recur (push root (dissoc o ::next)) (::next o))
              (loop [root (push root o)
                     i    i]
                (if (contains? i ::next)
                  (recur (push root (dissoc i ::next)) (::next i))
                  (push root i))))))))))

(defn attach [env form]
  (cond
    (vector? form) (into [] (map (partial attach env)) form)
    (map? form)    (assoc form ::env env)
    true           form))

;; OPTIMISE: This may benefit from memoisation.
(defn resolve [env {:keys [sym] :as im}]
  (let [s   (strip (ast/sym sym))
        env (if (contains? sym ::env) (::env sym) env)]
    (loop [{:keys [bindings] :as env} env]
      (cond
        (= ::root env)         im
        (contains? bindings s) (let [next (get bindings s)] ; `next` might be `false`!
                                 (attach next (merge-stacks (::env next) (::previous env))))
        true                   (recur (::previous env))))))

(defn local [x]
  (when (map? x)
    (::env x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; μ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def μ-env {::μ? true})

(defn μ-ctx? [env]
  (cond
    (= ::root env) false
    (::μ? env)     true
    true           (recur (::previous env))))

(defn capture [args env]
  (let [names (mapv strip (map ast/sym (butlast args)))
        body  (last args)
        env   (or (::env body) env)]
    (when (every? ast/unresolved? names)
      (conj names (attach (push env μ-env) body)))))

(defn bindargs
  "Returns `:body` of `μ` wrapped in a new env which unbinds the μ-env from
  `:body` and replaces it with the call frame."
  [env {:keys [name params body] :as μ} args]
  (assert (::μ? (::env body))) ; should be invariant
  (let [inner   (::previous (::env body)) ; remove μ-env frame.
        binding {:bindings (merge {params args} (when name {name μ}))}]
    (attach (push (merge-stacks inner env) binding) body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ctx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn with-channels [chs env form]
  {:style/indent 2}
  (let [env (or (::env form) env)]
    (attach (push env {:ctx chs}) form)))

;; OPTIMISE: This may benefit from memoisation.
(defn get-channel [env k]
  (when (not= ::root env)
    (if-let [ch (get-in env [:ctx k])]
      ch                               ; ch = false would be an error
      (recur (::previous env) k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; test cases
;; TODO: real tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def f (reduce push empty-env [{:a 1} {:b 2} {:c 4}]))
;; (def g (reduce push empty-env [{:a 1} {:b 2} {:d 5} {:e 7}]))
;; (def g' (push g {:test 42}))

;; (assert (= (merge-stacks f f) f))
;; (assert (= (merge-stacks g g') (merge-stacks g' g) g'))

;; (assert (= 4 (:c (merge-stacks f g))))
;; (assert (= 7 (:e (merge-stacks g f))))
