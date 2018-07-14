(ns algorithm-data-structure.data-structures.stack
  (:require [algorithm-data-structure.data-structures.linked-list :as llist]))

(defn create []
  (:linked-list (llist/create)))

(defn empty? [stack]
  (get-in stack [:linked-list :tail]))

(defn peek [stack]
  (when (seq stack)
    (get-in stack [:linked-list :tail :value])))

(defn push [stack value]
  (update-in stack [:linked-list]
             llist/append value))

(defn pop [stack]
  (let [removed-tail (llist/delete-tail (:linked-list stack))]
    (when removed-tail
      (:value removed-tail))))

(defn ->array [stack]
  (->> stack
       :linked-list
       llist/->array
       (map :value)
       reverse))

(defn ->string [stack callback]
  (llist/->string (:linked-list stack) callback))
