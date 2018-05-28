(ns algorithm-data-structure.data-structures.linked-list-node)

(defn create [value next]
  {:value value
   :next next})

(defn ->string [llnode & [callback]]
  (if callback
    (callback (:value llnode))
    (str (:value llnode))))
