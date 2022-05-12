(ns kunagi.utils.debug)

(defonce ACTIVE (atom (or #?(:cljs (boolean goog.DEBUG)
                             :clj false)
                          false)))

(defn active?
  "If debug mode is currently active."
  []
  @ACTIVE)

(defn activate []
  (reset! ACTIVE true))

;; * Watchable items

(defonce ITEMS (atom {}))

(defn reg-item [k v]
  (swap! ITEMS (fn [items]
                 (update items k (fn [set]
                                   (conj (or set #{})
                                         v))))))

(defn unreg-item [k v]
  (swap! ITEMS (fn [items]
                 (update items k (fn [set]
                                   (disj set v))))))
