(ns build
  (:refer-clojure :exclude [update])
  (:require
   [clojure.tools.build.api :as b]


   [kunagi.build.api :as kb :refer [print-task print-done print-debug]]
   ))

(defn release [{:keys []}]
  (kb/assert-git-clean)
  (kb/assert-deps-edn-has-no-local-deps!)
  (kb/run-tests)
  (kb/git-tag-with-version!)
  (kb/bump-version--bugfix!)
  )
