(ns xprl.env
  (:require
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace!]]))

;; TODO: Rewrite this entire ns. It's just a mess.
(def empty-ns
  {})

(defn set-ns [ns body]
  (trace! "ns replace" (sort-by :names (keys ns)))
  (assert (every? ast/unresolved? (keys ns)) ns)
  (walk/postwalk #(if (contains? ns %) (get ns %) %) body))

(defn strip [m lex]
  (reduce dissoc m (keys (:bindings lex))))

(defn stripμ [m {:keys [name params]}]
  (dissoc m name params))

(defn capture [form subs]
  (cond
    (empty? subs)   form ; nothing to do
    (ast/lex? form) (update form :form capture (strip subs form))
    (ast/μ? form)   (update form :body capture (stripμ subs form))


    ))

(defn rep [subs] (fn [form] (if (contains? subs form) (get subs form) form)))

(defn crep [subs]
  (fn [form]
    (let [s (ast/sym form)]
      (if (and (not (nil? s)) (contains? subs s))
        (get subs s)
        form))))

(defn sym-replace [form t subs]
  (let [finder ((case t :capture crep :bind rep) subs)]
    (cond
      (empty? subs)       form ; nothing to do
      (ast/barrier? form)  ; n
      (ast/symbol? form)  (let [next (finder form)]
                           (if (= next form)
                             (if (and #_(= t :bind) (ast/resolved? form))
                               (update form :val sym-replace t subs)
                               form)
                             next))
      (ast/μ? form)       (update form :body sym-replace t (strip subs form))
      true                (walk/walk #(sym-replace % t subs) finder form))))

(defn ns-intern [ns sym val]
  (assert (ast/unresolved? sym) sym)
  (assoc ns sym val))

(defn lookup [env sym]
  (assert (ast/unresolved? sym) sym)
  (get env sym))

(defn capture [args]
  ;; FIXME: Capture should *never* propagate into resolved symbols. If a value
  ;; was set before being passed here, it's set forever!

  ;; Capture is subtle. Hell it's the finickiest aspect of an obstruse language.
  ;; Let's try to summarise the problem:
  ;;
  ;; When we create a μ, we look at the parameter symbol and create a new
  ;; "captured" symbol which is a Resolved, but resolves to `nil` (ugly, yes.
  ;; practical, yes). We then walk what will be the body of the μ and replace
  ;; every occurance of the original symbol with the new captured symbol.
  ;;
  ;; But that's only the beginning. What if the symbol already resolves to
  ;; something else? Let's look at some cases:
  ;;
  ;; Symbol resolved at namespace level. If there's something called `x` in the
  ;; namespace we're defining an expression in, then any μ which uses `x` as its
  ;; parameter will find its body full of resolved xs. These should be
  ;; unresolved and then replaced with the captured `x`.
  ;;
  ;; If we have nested μs which all use (say) `x` as their parameter, then we
  ;; need to make sure an inner μ uncaptures and recaptures any `x`s in its body
  ;; which we're captured by surrounding μs.
  ;;
  ;; There are two subcases of the above. If the μs are all statically ready,
  ;; meaning the parameters are all known to be `x` from the getgo, then we
  ;; create the μs from the outside in and each wipes out its parent's bindings
  ;; from its own body. Easy peasy.
  ;;
  ;; If, on the other hand, some of the μ's are underspecified, meaning that we
  ;; don't know what their parameters will be yet, then the nested μs might be
  ;; created in any order. To prevent outer μs from recapturing the symbols of
  ;; inner μs, we have to be careful while walking the ast; if we come to a μ
  ;; whose parameter has the same root symbol as what we're replacing, then we
  ;; stop and leave that μ inviolate. If, however our parameter and the μ's
  ;; parameter have distinct roots, then we proceed as normal.
  ;;
  ;; There's one more case to consider when capturing. What if we come across a
  ;; resolved `x` which comes from a different context? I.e. is part of an
  ;; expression applied as argument to a higher μ?
  ;;
  ;; In this case we need to keep the original binding since messages must
  ;; retain the context in which they were sent to have any meaning.
  ;;
  ;; But how do we distinguish locally resolved symbols (symbols which were
  ;; params of parent μs and have been bound to args), from namespace resolved
  ;; symbols, from messages?
  ;;
  ;; We have three contexts of resolution: namespace, local, and message, and as
  ;; of yet there is no way to distinguish any of them from one another.
  ;;
  ;; Just by writing that out a solution is rather obvious. But I so dislike
  ;; adding more types to an already complicated system...
  (let [syms (mapv ast/capture (butlast args))
        subs (apply hash-map (interleave (map ast/unresolve (butlast args)) syms))
        next (conj syms (sym-replace (last args) :capture subs))]
    (trace! "capture" subs "\n" args "\n-->\n" next)
    next))

(defn bind [{:keys [name params body] :as μ} args]
  ;; Binding works by exact match. This is important since we don't want lexical
  ;; capture.
  ;;
  ;; Every captured symbol contains a unique tag to make sure that `x` in one
  ;; context doesn't conflict with `x` in any other context. Basically all
  ;; variables in the core language are `x`, so this is heavily exercised.
  ;; However, debugging errors is still a royal pain.
  ;;
  ;; I've been tempted in the past to gensym all local variables and make
  ;; shadowing an error. I know Roc does that, but it still stinks of a compiler
  ;; writer offloading their work onto the programmers of the language. If I
  ;; can't solve this with traditional lexical shadowing, but no traditional
  ;; lexical capture, then maybe I'll have to go that route... ick.
  (let [subs (merge {params (ast/resolve params args)}
                    (when name {name (ast/resolve name μ)}))
        _    (assert (every? ast/captured? (keys subs)))
        next (sym-replace body :bind subs)]
    (trace! "bind" (into {} (map (fn [[k v]] [k (:val v)])) subs) "\n" body " \n-->\n" next)
    next))


(defn bind [{:keys [name params body] :as μ} args]
  (ast/lex (merge {params args} (when name {name μ})) body))
