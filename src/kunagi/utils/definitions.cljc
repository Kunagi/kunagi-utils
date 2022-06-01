(ns kunagi.utils.definitions
  (:require
   [kunagi.utils :as u]))

(defonce DEFINITIONS (atom {}))

(defn definitions
  [type]
  (vals (get @DEFINITIONS type)))

(defn definition
  [type id]
  (get-in @DEFINITIONS [type id]))

(defn reg-definition [definition]
  ;; (prn "reg-definition" definition)
  (assert (map? definition))
  (let [id (-> definition :definition/id)
        type (-> definition :definition/type)]
    (assert (some? type) (str "Missing key :definition/type in definition: "
                              definition
                              #_(u/->edn definition)))
    (assert (keyword? type))
    (assert (some? id))
    (assert (or (keyword? id)
                (string? id)))
    (swap! DEFINITIONS assoc-in
           [type id]
           definition)
    definition))

#?(:clj
   (defn- complete-definition [definition sym type]
     (let [symbol-name (-> sym name)
           calling-namespace-name (name (ns-name *ns*))
           id (str calling-namespace-name "/" symbol-name)]
       (assoc definition
              :definition/type type
              :definition/id (keyword id)
              :definition/namespace-name calling-namespace-name
              :definition/symbol-name symbol-name
              :definition/qualified-symbol-name id))))

#?(:clj
   (defn macro-gen-def [sym opts type init-fn]
     (let [opts (-> opts
                    (complete-definition sym type))]
       (if init-fn
         `(def ~sym (reg-definition (~init-fn ~opts)))
         `(def ~sym (reg-definition ~opts))))))
