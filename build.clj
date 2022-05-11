(ns build
  (:refer-clojure :exclude [update])
  (:require
   [clojure.tools.build.api :as b]

   [kunagi.build.api :as kb]
   ))

(defn release [{:keys []}]
  (kb/release {:project 'kunagi-build})
  (kb/release {:project 'kunagi-utils})
  )
