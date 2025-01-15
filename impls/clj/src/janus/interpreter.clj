(ns janus.interpreter
  "This namespace only exists to mitigate circular dependencies."
    (:refer-clojure :exclude [eval apply reduce])
)

(defn event! [type x dyn ccs]
  ;; FIXME: This should log different fields based on type and x.
  ;; Why is the type of x nil when x is a record from AST?
  #_(t/event! (keyword (.name *ns*) (str/join "." [(name type) (.getName (type x))]))
            {:level :trace
             :kind  ::trace
             :dyn   dyn
             :form  x}))


(defprotocol Reduce
  (reduce* [x dyn ccs]))

(defn reduce{:style/indent 2} [f dyn ccs]
  (event! :reduce f dyn ccs)
  (reduce* f dyn ccs))

(defprotocol Eval
  (eval* [x dyn ccs]))

(defn eval {:style/indent 2} [x dyn ccs]
  (event! :eval x dyn ccs)
  (eval* x dyn ccs))

(defprotocol Apply
  (apply* [head tail dyn ccs]))

(defn apply {:style/indent 3} [head tail dyn ccs]
  (event! :apply [head tail] dyn ccs)
  (apply* head tail dyn ccs))
