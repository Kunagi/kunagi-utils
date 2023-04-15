(ns kunagi.utils.test
  #?(:cljs (:require-macros [kunagi.utils.test :refer [def-itest]]))
  (:require
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])
   [clojure.edn :as edn]
   [clojure.string :as str]
   [kunagi.utils :as u]
   [kunagi.utils.rct :refer [rct]]
   [promesa.core :as p]
   [promesa.exec :as px]))

(defonce ITESTS (atom {}))

;; (defn- transform-itest-body [s-exps]
;;   (let [[next-exp rest-exps] s-exps]
;;     '(let [])
;;   )

(defmacro wrap-expr [expr]
  `(let [report# {:expr '~expr}]
     (try
       (let [result# ~expr]
         [result# (assoc report#
                         :result result#
                         :success true)])
       (catch :default ex#
         [nil (assoc report#
                     :error ex#)]))))



(defmacro def-itest
  {:clj-kondo/lint-as 'clojure.test/deftest}
  [sym & body]
  (let [ns-name (name (ns-name *ns*))
        var-name (name sym)
        id (str ns-name "/" var-name)
        qualified-sym (symbol ns-name var-name)
        ;; [opts body]
        report-sym (gensym "report_")
        bindings (->> body
                      (mapcat (fn [expr]
                                (let [def? (and (sequential? expr)
                                                (-> expr first (= 'def)))
                                      sym (if def?
                                            (-> expr second)
                                            (gensym "s_"))
                                      [sym expr] (if def?
                                                   [sym (nth expr 2)]
                                                   [sym expr])]
                                  `[[~sym expr-report#] (if (-> ~report-sym :error)
                                                          [nil {:expr '~expr}]
                                                          (wrap-expr ~expr))
                                    expr-report# (assoc expr-report#
                                                        :var-name ~(when def?
                                                                     (str sym)))
                                    ~report-sym (-> ~report-sym
                                                    (update :exprs conj expr-report#)
                                                    (assoc :error (or (-> expr-report# :error)
                                                                      (-> ~report-sym :error))))]))))
        f `(fn []
             (p/let [~report-sym {:exprs []}
                     ~@bindings]
               ~report-sym))
        itest {:sym `'qualified-sym
               :run-f f
               :run-f-body (u/->edn body)
               :run-f-wrapped (u/->edn f)}]
    `(swap! ITESTS assoc '~qualified-sym ~itest)))

(defn run-itest> [itest]
  (when itest
    (tap> [:itest itest])
    (p/let [_ nil
            f> (-> itest :run-f)]
      (f>))))
