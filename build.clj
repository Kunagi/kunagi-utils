(ns build
  (:refer-clojure :exclude [update])
  (:require
   [clojure.tools.build.api :as b]


   [kunagi.build.api :as kb :refer [print-task print-done print-debug]]
   ))

(defn assert-git-clean [{:keys []}]
  (kb/assert-git-clean)
  )

(defn release [{:keys []}]
  (kb/release! {})
  )
