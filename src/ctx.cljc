(ns ctx
  (:require
   [promesa.core :as p]))

(defmulti custom-edn pr-str)

(defrecord Ref [type id])

(defn ref? [thing]
  (instance? Ref thing))

(defrecord Ctx [])

#_
(defmethod custom-edn Ref [r]
  (str "#ctx.Ref " (pr-str [(:type r) (:id r)])))

(defmulti ref-resolve> (fn [ref]
                         (-> ref :type)))

(defn resolve> [ref]
  (when ref
    (assert (ref? ref))
    (p/let [resolved (ref-resolve> ref)]
      resolved)))
