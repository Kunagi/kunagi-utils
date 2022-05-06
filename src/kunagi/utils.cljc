(ns kunagi.utils
  (:require
   [hyperfiddle.rcf :refer [tests ! %]]))

;; * numbers

(defn parse-float [v]
  (when v
    #?(:cljs
       (let [f (js/parseFloat v)]
         (when (js/isNaN f) (throw (ex-info (str "NaN: " v)
                                            {:value v})))
         f)
       :clj (Float/parseFloat v))))


(tests
  ;; (hyperfiddle.rcf/enable!)
  (str (parse-float "22.2")) := "22.2"
  ;; (parse-float "22.2x")
  ;; (parse-float "c22.2")
  "done"
  )
