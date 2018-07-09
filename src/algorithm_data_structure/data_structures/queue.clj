(ns algorithm-data-structure.data-structures.queue
  (:require [algorithm-data-structure.data-structures.linked-list :as llist]))

(defn create []
  {:linked-list (llist/create)})

(defn empty? [queue]
  (-> queue :linked-list :tail not))

(defn peek [queue]
  (when-let [h (-> queue :linked-list :head)]
    (:value h)))

(defn enqueue [queue value]
  (update queue :linked-list append value))

(defn dequeue [queue value]
  (let [[llist h] (-> queue :linked-list delete-head)]
    [{:linked-list llist} (when h (:value h))]))

(defn ->string [queue callback]
  (llist/->string (:linked-list queue) callback))
