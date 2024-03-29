(ns kunagi.utils
  (:refer-clojure :exclude [text])
  #?(:cljs (:require-macros [kunagi.utils :refer [try>]]))
  (:require
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])
   [clojure.edn :as edn]
   [clojure.string :as str]
   [kunagi.utils.rct :refer [rct]]
   [promesa.core :as p]
   [promesa.exec :as px]))

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

(defn error-userfriendlyfy-message [s]
  (when s
    (->> s
         str/split-lines
         (remove #(str/starts-with? % "    at "))
         (str/join "\n"))))

(defn error-user-message [error]
  (assert (map? error))
  (if-let [cause (-> error :cause)]
    (str (error-user-message cause)
         " ➤ " (-> error :message error-userfriendlyfy-message))
    (-> error :message error-userfriendlyfy-message)))

;; * try>

(defn promise? [thing]
  (when thing
    (or #?(:cljs (instance? js/Promise thing))
        (p/promise? thing))))

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
  (assert (fn? f))
  (p/create (fn [resolve reject]
              (px/schedule! wait-millis
                            (fn []
                              (try
                                (resolve (f))
                                (catch #?(:cljs :exception :clj Exception) ex
                                  (reject ex))))))))

(rct later>-test
     (let [*1 (later> 100 (fn [] 42))
           _ (assert (= *1 42))])
     )

;;; time

(defn current-time-millis []
  #?(:cljs (-> (js/Date.) .getTime)
     :clj (System/currentTimeMillis)))

;;; Texts


(defonce LOCAL_TEXTS (atom {}))

(defn get-text [k de-text arg-map]
  (when-let [s (get @LOCAL_TEXTS k de-text)]
    (if-not arg-map
      s
      (->> arg-map
           (reduce (fn [s [k v]]
                     (str/replace s (str "$" (str/upper-case (name k)))
                                  v))
                   s)))))

#?(:clj
   (let [file-path "src/texts_de.edn"]
     (defonce DE_TEXTS (do
                         (prn "[kunagi.utils/text] loading " file-path)
                         (atom (try
                                 (read-edn (slurp file-path))
                                 (catch Exception _)))))

     (defn register-de-text [k text]
       (when (not= text (get @DE_TEXTS k))
         (do
           (prn "[kunagi.utils/text] new text: " k text)
           (swap! DE_TEXTS assoc k text)
           (spit file-path (->edn @DE_TEXTS)))))

     (defmacro text [k de-text & [arg-map]]
       (assert (simple-keyword? k))
       (assert (string? de-text))
       (register-de-text k de-text)
       `(get-text ~k ~de-text ~arg-map))))
