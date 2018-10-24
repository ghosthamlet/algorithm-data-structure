(ns algorithm-data-structure.data-structures.stack
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/stack"
  (:require [algorithm-data-structure.data-structures.linked-list :as ll]))

(defn create []
  {:linked-list (ll/create)})

(defn empty?* [self]
  (not (get-in self [:linked-list :tail])))

(defn peek* [self]
  (when (seq self)
    (get-in self [:linked-list :tail :value])))

(defn push [self value]
  (update-in self [:linked-list] ll/append value))

(defn pop* [self]
  (let [[_ removed-tail] (ll/delete-tail (:linked-list self))]
    (when removed-tail
      (:value removed-tail))))

(defn ->array [self]
  (->> self
       :linked-list
       ll/->array
       (map :value)
       reverse))

(defn ->string [self callback]
  (str (vec (ll/->string (:linked-list self) callback))))
