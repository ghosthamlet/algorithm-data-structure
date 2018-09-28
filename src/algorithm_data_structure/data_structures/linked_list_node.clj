(ns algorithm-data-structure.data-structures.linked-list-node
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/linked-list")

(defn create [value & [next]]
  {:value value
   :next next})

(defn ->string [self & [callback]]
  (if callback
    (callback (:value self))
    (str (:value self))))
