(ns xprl.compiler
  (:refer-clojure :exclude [eval apply resolve compile]))


(defn application )

(defn apply
  ([form env] (apply (:head form) (:tail form) env))
  ([head tail env]
   (cond
     (ast/external? head) (head form env)
     (ast/μ? head)        (compile-μ-apply head tail env)
     :else                (throw (Exception. (str "bad application:" head "," tail))))))

(defn eval [form env]
  (cond
    (ast/symbolic? form) (resolve form env)
    (ast/pair? form)     (compile (ast/immediate (:head form)) env
                                  {:call :apply
                                   :args (:tail env)
                                   :env  env})))

;; this is going to be a fairly standard stacked env interpreter which
;; interprets forms into an intermediate representation. So a compiler... more
;; or less.
(defn compile [form state]
  (cond
    (ast/immediate? form)   (eval form state)
    (ast/application? form) (apply form state)
    :else                   form))


(defn createμ [form env conts])



;; =>
{:return ???
 %9      [:eval %14 %11]
 %14     (+* ~x 1)
 %10     [:nth* %4 2]
 %11     [:capture :env %12]
 %12     [:compile %13 :env]
 %13     [:nth* %4 1]
 %4      [:join %5 %6]
 %5      x
 %6      ~(+* ~x 1)}
;; =>
{:return ???
 %9      [:apply %15 %16 %11]
 %15     ~+* ; [:immediate [:head %14]]
 %14     (+* ~x 1)
 %16     [:join %17 %18]
 %17     ~x
 %18     1
 %10     [:nth* %4 2]
 %11     [:capture :env %12]
 %12     [:compile %13 :env]
 %13     [:nth* %4 1]
 %4      [:join %5 %6]
 %5      x
 %6      ~(+* ~x 1)}
;; =>
{:return ???
 %9      [F+* %16 %11]
 %16     [:join %17 %18]
 %17     ~x
 %18     1
 %11     {:captures #{x}}}
;; =>
{:return ???
 %9      [F+** %17]
 %17     [:join %18 %19]
 %18     [:compile %17 %11]
 %19     1
 %11     {:captures #{x}}}
;; =>
{:return ???
 %9      [F+** %17]
 %17     [:join %18 %19]
 %18     [:resolve x {:catures #{x}}]
 %19     1}
;; =>
{:return %9
 %9      [F+** %17]
 %17     [:join %18 %19]
 %18     [:wait x]
 %19     1}
;; =>
{:return [F+** %18 %19]
 %18     [:wait x]
 %19     1}

{:return [Fnth* %2 %3]
 %2      [:vec %4 %5]
 %4      [Fnth* %6 2]
 %6      :input
 %5      [Fnth* %7 3]
 %7      :input
 %3      [-* 2 %8]
 %8      [get* %9 %10] ; this whole get can be optimised away to 1 instruction
 %9      {false 0 true 1} ; data selection
 %10     [nth* %11 1]
 %11     :input}
;; `:input` will be the same so this simplifies to:
{:return [Fnth* %2 %3]
 %2      [:vec %4 %5]
 %4      [Fnth* %11 2]
 %5      [Fnth* %11 3]
 %3      [-* 2 %8]
 %8      [get* %9 %10]
 %9      {false 0 true 1}
 %10     [nth* %11 1]
 %11      :input}
