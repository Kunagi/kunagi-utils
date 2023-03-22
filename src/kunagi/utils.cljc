(ns kunagi.utils
  #?(:cljs (:require-macros [kunagi.utils :refer [try>]]))
  (:require
   [clojure.string :as str]
   [clojure.edn :as edn]
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])

   [promesa.exec :as px]

   [kunagi.utils.rct :refer [rct]]))

;; * strings

(defn non-blank-string [s]
  (when (string? s)
    (when-not (str/blank? s)
      s)))

;; * EDN

(defn ->edn [data]
  (with-out-str (pprint data)))

(defn read-edn [s]
  (when s
    (edn/read-string s)))

;; * numbers

(defn parse-float [v]
  (when v
    #?(:cljs
       (let [f (js/parseFloat v)]
         (when (js/isNaN f) (throw (ex-info (str "NaN: " v)
                                            {:value v})))
         f)
       :clj (Float/parseFloat v))))

(rct parse-float-test
     (let [_ (assert (= (str (parse-float "22.2")) "22.2"))
           _ (assert (= (str (parse-float "23")) "23"))]))

;; * errors

(defn error->text [ex]
  (cond

    (nil? ex)
    nil

    (string? ex)
    ex

    #?(:cljs (instance? js/Error ex))
    #?(:cljs (str ""
                  (-> ^js ex .-message)
                  (when-let [c (ex-cause ex)]
                    (str "\n"
                         (error->text c)))))

    (map? ex)
    (if-let [message (-> ex :message)]
      (str ""
           message
           (when-let [c (ex-cause ex)]
             (str "\n"
                  (error->text c))))
      "Error")

    :else
    (->edn ex)))

(defn error->data [ex]
  (cond

    (nil? ex)
    {:message "nil"
     :type :nil}

    (string? ex)
    {:message ex
     :type :string}

    #?(:cljs (instance? js/Error ex))
    #?(:cljs {:message (-> ^js ex .-message)
              :type :js/Error
              :data (ex-data ex)
              :stacktrace (-> ^js ex .-stack)
              :cause (when-let [c (or (ex-cause ex)
                                      (-> ^js ex .-cause))]
                       (error->data c))})

    (map? ex)
    {:message (or (-> ex :message)
                  "data")
     :type (or (-> ex :type)
               :map)
     :data (dissoc ex :message :stacktrace :cause)
     :stacktrace (-> ex :stacktrace)
     :cause (when-let [cause (-> ex :cause)]
              (error->data cause))}

    #?(:cljs
       (object? ex))
    #?(:cljs
       {:message "JavaScript Object"
        :type :js-object
        :data (-> ex
                  js->clj)})

    :else
    {:data (->edn ex)}))

(comment
  (instance? js/Error
             (ex-cause
              (ex-info "ex-info error"
                       {:with :data}
                       (js/Error. "Some cause"))))

  (:cause
   (error->data
    (ex-info "ex-info error"
             {:with :data}
             (js/Error. "Some cause"))))
  ;
  )

(defn error-user-message [error]
  (assert (map? error))
  (if-let [cause (-> error :cause)]
    (str (error-user-message cause)
         " ➤ " (-> error :message))
    (-> error :message)))

;; * try>

#?(:clj
   (defmacro try> [expr & [catch-f]]
     (let [expr-code (->edn expr)
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
(defn later> [wait-millis f]
  (px/schedule! wait-millis f))

;; * time

(defn current-time-millis []
  #?(:cljs (-> (js/Date.) .getTime)
     :clj (System/currentTimeMillis)))
