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

(defn get-nodes-by-value [m value]
  (map #(get-node m (first %))
       (filter #(= (:value (second %)) value) m)))

(defn get-next [node] (get-node (:m node) (:next node)))
(defn get-prev [node] (get-node (:m node) (:previous node)))

(defn ->string [self callback]
  (if callback
    (callback (:value self))
    (str (:value self))))
