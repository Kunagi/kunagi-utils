(ns build
  (:refer-clojure :exclude [update])
  (:require
   [clojure.tools.build.api :as b]


   [kunagi.build.api :as kb :refer [print-task print-done print-debug]]
   ))

(defn release [{:keys []}]
  (kb/release-2 {:project 'kunagi-build})
  (kb/release-2 {:project 'kunagi-utils})
  )
