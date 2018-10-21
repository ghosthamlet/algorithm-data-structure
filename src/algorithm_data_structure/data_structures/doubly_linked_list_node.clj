(ns algorithm-data-structure.data-structures.doubly-linked-list-node
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/doubly-linked-list")

(defrecord Node [previous next value])

(defn create
  ([value] (create nil value))
  ([next value] (create nil next value))
  ([previous next value] (Node. previous next value)))

(defn get-node [m key]
  (when-let [node (get m key)]
    (assoc node :m m :key key)))

(defn get-next [node] (create (:m node) (:next node)))
(defn get-prev [node] (create (:m node) (:previous node)))

(defn ->string [self callback]
  (if callback
    (callback (:value self))
    (str (:value self))))
