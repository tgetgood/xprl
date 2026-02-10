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


{:return [Fnth* %2 %3]
 %2      [:vec %4 %5]
 %4      [Fnth* %6 2]
 %6      :input
 %5      [Fnth* %7 3]
 %7      :input
 %3      [-* 2 %8]
 %8      [get* %9 %10] ; this whole get* can be optimised away to 1 instruction
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

;; wrap
{:return [:compile %1 {}]
 :%1     ~(μ f ~(μ args ~(~f . ~~args)))}

{:return [:apply %3 %4 {}]
 :%3     ~μ
 %4      [:join %5 %6]
 %5      f
 %6      ~(μ args ~(~f . ~~args))}

{:return [Fμ %5 %6 {}]
 %5      f
 %6      ~(μ args ~(~f . ~~args))}

{:return [μwrap %7 %8]
 %7      [:compile %6 {:capture {f %8}}]
 %8      [gensym]
 %6      ~(μ args ~(~f . ~~args))
 }

{:return [μwrap %7 %8]
 %7      [Fμ %9 %10 {:capture {f %8}}]
 %9      args
 %10     ~(~f . ~~args)
 %8      [gensym]
 %6      ~(μ args ~(~f . ~~args))

 }
