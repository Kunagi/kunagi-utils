(ns kunagi.utils.rct
  #?(:cljs (:require-macros [kunagi.utils.rct :refer [rct]]))

  (:require
   [promesa.core :as p]
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])))

(defn- current-time-millis []
  #?(:cljs (-> (js/Date.) .getTime)
     :clj (System/currentTimeMillis)))


(defonce AUTORUN (atom false))
(defn autorun? [] @AUTORUN)

(defn enable-autorun!
  ([]
   (enable-autorun! true))
  ([autorun-on?]
   (reset! AUTORUN autorun-on?)))

;; #?(:clj
;;    (defn- compiler-option [k]
;;      (when cljs.env/*compiler*
;;        (get-in @cljs.env/*compiler* [:options k]))))


(defonce RCTS (atom {}))

(defn add-rct [ns-name var-name f bindings-form]
  (prn "[add-rct]" ns-name var-name f bindings-form)
  (assert (string? ns-name))
  (assert (string? var-name))
  (assert (fn? f))
  (let [id (str ns-name "/" var-name)
        bindings-form-before (get-in @RCTS [id :bindings-form])
        bindings-form-changed? (not= bindings-form bindings-form-before)
        _ (tap> [:changed? bindings-form-changed?
                 bindings-form bindings-form-before])
        tsm (current-time-millis)
        rct {:id id
             :f f
             :bindings-form bindings-form
             :tsm-def tsm
             :tsm-def-changed (if bindings-form-changed?
                                 tsm
                                 (get-in @RCTS [id :tsm-def-changed]))}]
    (swap! RCTS assoc id rct)))

(defn eval-rct> [rct]
  (p/let [f (-> rct :f)
          result (f)]
    (assoc rct
           :result result
           :tsm-eval (current-time-millis))))

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
          (add-rct ~ns-name ~var-name f# '~bindings)))))

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
