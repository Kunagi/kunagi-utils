(ns kunagi.utils.hiccup
  #?(:cljs (:require-macros [kunagi.utils.hiccup :refer []]))
  (:require
   #?(:cljs [goog.string :as gstring])
   [kunagi.utils :as u]

   [clojure.string :as str])
  )

(defn escape-html [context s]
  (if s
    #?(:cljs (gstring/htmlEscape (str s))
       :clj s) ;; FIXME
    ""))

(def unclosed-tags
  #{:img :br :meta :hr})

(defn ->html [context thing]
  (cond

    (nil? thing)
    ""

    (and (vector? thing)
         (-> thing first (= :html>)))
    (second thing)

    (vector? thing)
    (let [[tag & args] thing
          _ (assert (simple-keyword? tag))
          [opts & children] args
          [opts children] (if (or (nil? opts)
                                  (map? opts))
                            [opts children]
                            [nil (into [opts children])])
          unclosed-tag? (contains? unclosed-tags tag)]
      (str "\n<" (name tag)
           (when (seq opts)
             (->> opts
                  (map (fn [[k v]]
                         (str " " (name k) "=\"" (escape-html context v) "\"")))
                  (str/join "")))
           ">"
           (when-not unclosed-tag?
             (str
              (->> children
                   (remove nil?)
                   (map (fn [child]
                          (->html context child)))
                   (str/join ""))
              "</" (name tag) ">"))))

    (sequential? thing)
    (->> thing
         (map #(->html context %))
         (str/join ""))

    :else
    (escape-html context (str thing))))

(comment
  (->html {} "hello world")
  (->html {} [:h1 "hello"])
  (->html {} [:div [:div "hello"]])
  (->html {} [:div {:style "font-weight: bold;"} "hello"])
  (->html {} [:div
              [:div "a"]
              [:div "b"]])
  ;;
  )
