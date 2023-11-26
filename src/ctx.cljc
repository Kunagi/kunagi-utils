(ns ctx
  (:require
   [promesa.core :as p]
   [kunagi.utils.logging :refer [log]]))

(defrecord Ref [type id])

(defn ref? [thing]
  (instance? Ref thing))

(defrecord Ctx [m])

(defn ctx? [thing]
  (instance? Ctx thing))

(defmulti ref-resolve> (fn [ref ctx]
                         (-> ref :type)))

(defn resolve> [ref ctx]
  (when ref
    (assert (ref? ref))
    (assert (ctx? ctx))
    (p/let [resolved (ref-resolve> ref ctx)]
      resolved)))

;;; state atom

(defonce CTX (atom nil))

(defn update! [log-entry f]
  (log ::update!
       :log log-entry)
  (swap! CTX (fn [ctx]
               (let [new-ctx (f ctx)]
                 (assert (ctx? new-ctx))
                 new-ctx))))
