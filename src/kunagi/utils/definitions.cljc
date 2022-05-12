(ns kunagi.utils.definitions)

(defonce DEFINITIONS (atom {}))

(defn definitions
  [type]
  (get @DEFINITIONS type))

(defn reg-definition [type id thing]
  (assert (keyword? type))
  (assert (or (keyword? id)
              (string? id)))
  (swap! DEFINITIONS assoc-in [type id] thing))
