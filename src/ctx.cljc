(ns ctx
  (:require
   [promesa.core :as p]))

(defmulti custom-edn pr-str)

(defrecord Ref [type id])

(defn ref? [thing]
  (instance? Ref thing))

(defrecord Ctx [])

(defn ctx? [thing]
  (instance? Ctx thing))

#_
(defmethod custom-edn Ref [r]
  (str "#ctx.Ref " (pr-str [(:type r) (:id r)])))

(defmulti ref-resolve> (fn [ref ctx]
                         (-> ref :type)))

(defn resolve> [ref ctx]
  (when ref
    (assert (ref? ref))
    (assert (ctx? ctx))
    (p/let [resolved (ref-resolve> ref ctx)]
      resolved)))
