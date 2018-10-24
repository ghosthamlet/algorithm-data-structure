(ns algorithm-data-structure.data-structures.queue
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/queue"
  (:require [algorithm-data-structure.data-structures.linked-list :as ll]))

(defn create []
  {:linked-list (ll/create)})

(defn empty?* [self]
  (-> self :linked-list :tail not))

(defn peek* [self]
  (-> self :linked-list :head :value))

(defn enqueue [self value]
  (update self :linked-list ll/append value))

(defn dequeue [self]
  (let [[ll h] (-> self :linked-list ll/delete-head)]
    [{:linked-list ll} (:value h)]))

(defn ->string [self callback]
  (str (vec (ll/->string (:linked-list self) callback))))
