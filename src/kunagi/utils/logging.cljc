(ns kunagi.utils.logging
  #?(:cljs (:require-macros [kunagi.utils.logging :refer [log]]))
  (:require
   #?(:gcf ["firebase-functions" :as firebase-functions])
   #?(:cljs [cljs.pprint :as pprint]
      :clj [clojure.pprint :as pprint])
   [clojure.string :as str]
   [flatland.ordered.map :as ordered.map]))

;;; logging

#?(:gcf
   (defn log-with-gcf [event event-data]
     (try
       (.write ^js (.-logger firebase-functions)
               (clj->js
                {:severity "DEBUG"
                 :message (str (namespace event)
                               " | "
                               (name event))
                 :ns (namespace event)
                 :event (name event)
                 :data event-data}))
       (catch :default err
         (.write ^js (.-logger firebase-functions)
                 (clj->js
                  {:severity "DEBUG"
                   :message (str (namespace event)
                                 " | "
                                 (name event))
                   :ns (namespace event)
                   :event (name event)
                   :logging-error err}))))))

(defn log-with-println [event event-data]
  (println event (when event-data (with-out-str (pprint/pprint event-data)))))

(defn log-with-console [event event-data]
  #?(:clj (log-with-println (str (namespace event) "/" (name event)) event-data)
     :cljs (if event-data
             (js/console.log (str (namespace event) "/" (name event))
                             (clj->js event-data))
             (js/console.log (str (namespace event) "/" (name event))))))

;;; log macro

#?(:clj
   (defn compiler-option [k]
     (when-let [ENV (eval 'cljs.env/*compiler*)]
       (get-in @ENV [:options k]))))

(def col--kunagi "#8d6e63")
(def col--app "#6d4c41")
(def col--event "#0277bd")
(def col--event--tap> "#c43e00")
(def col--exception "#d32f2f")

(def css--ns--kunagi (str "background-color: " col--kunagi "; color: white; padding: 2px 4px; border-radius: 4px;"))
(def css--ns--app (str "background-color: " col--app "; color: white; padding: 2px 4px; border-radius: 4px;"))
(def css--event--default (str "background-color: " col--event "; color: white; font-weight: bold; padding: 2px 4px; border-radius: 4px; margin-left: 4px;"))
(def css--event--tap> (str "background-color: " col--event--tap> "; color: white; font-weight: bold; padding: 2px 16px; border-radius: 4px; margin-left: 4px;"))
(def css--exception (str "background-color: " col--exception "; color: white; font-weight: bold; padding: 2px 4px; border-radius: 4px; margin-left: 4px;"))

#?(:clj
   (defn ->log-expr [event event-data]

     (case (compiler-option :kunagi-logging-mode)

       :gcf
       ;; `(let [logger (-> firebase-funcotins .-logger)]
       ;;    (.log logger "[gcf]" ~event ~event-data))
       ;; `(.log js/console "[gcf]" ~event ~event-data)
       ;; `(.log (.-logger firebase-functions) "[gcf]" ~event ~event-data)
       `(log-with-gcf ~event ~event-data)

       :js-console
       (if event-data
         `(.log js/console
                "\n***>"
                ~(namespace event) " " ~(name event)
                "  <***"
                "\n"
                (cljs.core/clj->js ~event-data))
         `(.log js/console
                "***>"
                ~(namespace event) " " ~(name event)
                "  <***"))

       :browser-console
       (let [css--ns (if (or (-> event namespace (str/starts-with? "kunagi."))
                             (-> event namespace (str/starts-with? "spark.")))
                       (symbol "kunagi.utils.logging" "css--ns--kunagi")
                       (symbol "kunagi.utils.logging" "css--ns--app"))
             css--event (cond
                          (-> event name (= "tap>")) (symbol "kunagi.utils.logging" "css--event--tap>")
                          :else (symbol "kunagi.utils.logging" "css--event--default"))
             exception (-> event-data :exception)
             event-data (if exception
                          (dissoc event-data :exception)
                          event-data)
             event-expr (str "%c" (namespace event)
                             "%c" (name event)
                             (when exception
                               (str "%c:exception")))]

         `(.log
           js/console
           ~event-expr
           ~css--ns ~css--event
           ~@(when exception [css--exception "\n" exception "\n"])
           ~@[event-data]))

                                        ; else
       `(log-with-console ~event ~event-data)
       ;; nil
       ;; `(log-with-println ~event ~event-data)
       )))

#?(:clj (defn data-kvs->data [data-kvs]
          (when (seq data-kvs)
            (->> data-kvs
                 (partition 2)
                 (map #(->> % (into [])))
                 (into (ordered.map/ordered-map))))))

#?(:clj
   (defmacro log [event-keyword & data-kvs]
     (let [data (data-kvs->data data-kvs)]
       (->log-expr event-keyword data))))

(defonce REPORT_ERROR_F (atom (fn [event-keyword data-kvs]
                                (tap> {:error-event event-keyword
                                       :data data-kvs}))))

(defn report-error [event-keyword data-kvs]
  (try
    (@REPORT_ERROR_F event-keyword data-kvs)
    (catch #?(:cljs :default :clj Throwable) ex
      (prn "ERROR calling kunagi.utils.logging/REPORT_ERROR_F")
      (prn ex)
      (prn event-keyword data-kvs))))

#?(:clj
   (defmacro log-error [event-keyword & data-kvs]
     (let [data (data-kvs->data data-kvs)]
       `(do
          (report-error ~event-keyword ~data)
          (log ~event-keyword ~@data-kvs)))))

;;; tap

(defonce TAP (atom nil))

#?(:cljs
   (defn install-tap []
     (log ::install-tap)
     (swap! TAP (fn [old-tap]
                  (when old-tap
                    (remove-tap old-tap))
                  (let  [tap (fn [value]
                               (log ::tap> :_ value))]
                    (add-tap tap)
                    tap)))))

#?(:cljs
   (when goog.DEBUG
     (install-tap)))

;;; ---

(comment
  (log ::with-message
       :message "This is the message.\nMultiline!")
  (log :simple)
  (type (ex-info "hallo" {:x 1}))
  (instance? js/Error (ex-info "hallo" {}))
  (ex-data (ex-info "hey" {:p1 "h"}))
  (log ::dummy-with-data
       :param-1 "witek"
       :context {:this 1
                 :and "and that" :more [1 2 3 4 5] :asdlfjkasldfkj "asöldj aslödkj asöldkfj asöldfj "})
  (log (ex-info "Error here" {:with :data}))
  (log ::exception (ex-info "Error Here" {:with "data"}))
  (log ::auth-state-changed
       :user nil)
  (log ::test
       "missing key"))
