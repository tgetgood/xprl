(ns xprl.ns
  (:refer-clojure :exclude [resolve extend])
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [xprl.ast :as ast]
   [xprl.debug :refer [trace! record!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-ns {})

(defn ns-intern [ns sym val]
  (assert (not (contains? ns (ast/symbol sym))) (str sym " cannot be redefined."))
  (assoc ns (ast/symbol sym) val))

;; N.B.: This is used for tooling. Don't delete it.
(defn lookup [env sym]
  (assert (contains? env sym) sym)
  (get env sym))
