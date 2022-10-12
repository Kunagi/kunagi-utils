(ns kunagi.utils.rct
  #?(:cljs (:require-macros [kunagi.utils.rct :refer [rct]]))

  (:require
   [promesa.core :as p]
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])))

(defonce RCTS (atom {}))

(defn add-rct [ns-name var-name f bindings-form]
  (prn "[add-rct]" ns-name var-name f bindings-form)
  (assert (string? ns-name))
  (assert (string? var-name))
  (assert (fn? f))
  (let [id (str ns-name "/" var-name)
        rct {:id id
             :f f
             :bindings-form bindings-form}]
    (swap! RCTS assoc id rct)))

(defn eval-rct> [rct]
  (p/let [f (-> rct :f)
          result (f)]
    (assoc rct :result result)))

#?(:clj
   (defmacro rct [sym let-expr]
     (let [ns-name (name (ns-name *ns*))
           var-name (name sym)
           id (str ns-name "/" var-name)
           [let-sym bindings] let-expr
           elements (->> bindings
                         (partition 2)
                         (mapcat (fn [[bind-name expression]]
                                   `[[~bind-name element#] (p/handle
                                                            (p/create (fn [resolve# _reject#]
                                                                        (resolve# ~expression)))
                                                            (fn [value# err#]
                                                              [value#
                                                               {:id (str (random-uuid))
                                                                :bind-name (str '~bind-name)
                                                                :expression '~expression
                                                                :value value#
                                                                :eval-error err#}]))
                                     ~'elements (conj ~'elements element#)]))
                         (into ['elements []]))]
       `(let [f# (fn []
                   (p/let ~elements
                     {:elements ~'elements
                      :DEBUG '~elements}))]
          (add-rct ~ns-name ~var-name f# '~bindings)))
     #_(let [expr-code (->edn expr)
             ;; catch-f-expr (when catch-f
             ;;                `(~catch-f))
             ]
         `(try (-> ~expr
                   (.catch (fn [error#]
                             (let [catch-f# ~catch-f]
                               (when catch-f# (catch-f# error# {:expr ~expr-code}))
                               ;; (log-error ::try>--failed
                               ;;            :promise promise
                               ;;            :error (u/error->data error))
                               {:error error#
                                :expr  ~expr-code}))))
               (catch :default error#
                 (let [catch-f# ~catch-f]
                   (when catch-f# (catch-f# error# {:expr ~expr-code}))
                   (js/Promise.resolve {:error error#
                                        :expr ~expr-code})))))))

(prn "[rct] loading...")
(rct test-1
     (let [vorname "Witek"
           _ (assert (= vorname "Olga") "Vorname muss passen")]))
;; (rct test-1
;;      (let [_vorname (p/rejected "rejected")]))
;; (rct test-1
;;      (let [_vorname (throw (ex-info "boom" {}))]))
;; (rct test-1
;;      (let [_vorname (let [s (str "Wi" "tek")] s)
;;                       _nachname (str "Koczewski")]))
