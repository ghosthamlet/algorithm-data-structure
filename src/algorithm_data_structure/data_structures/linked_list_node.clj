(ns algorithm-data-structure.data-structures.linked-list-node)

(defn create [value & [next]]
  {:value value
   :next next})

(defn ->string [self & [callback]]
  (if callback
    (callback (:value self))
    (str (:value self))))
