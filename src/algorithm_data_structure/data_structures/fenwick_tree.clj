(ns algorithm-data-structure.data-structures.fenwick-tree)

(defn create [array-size]
  {:array-size array-size
   :tree-array (vec (repeat (inc array-size) 0))})

(defn increase [ft position value]
  (if (or (< position 1) (> position (:array-size ft)))
    (throw (Exception. "Position is out of allowed range"))
    (loop [i position
           ft ft]
      (if (> i (:array-size ft))
        ft
        (recur (+ i (bit-and i (- i)))
               (update-in ft [:array-size i] + value))))))

(defn query [ft position]
  (if (or (< position 1) (> position (:array-size ft)))
    (throw (Exception. "Position is out of allowed range"))
    (loop [i position
           sum 0]
      (if (zero? i)
        sum
        (recur (- i (bit-and i (- i)))
               (+ sum (get-in ft [:tree-array i])))))))

(defn query-range [ft left-index right-index]
  (if (> left-index right-index)
    (throw (Exception. "Left index can not be greater then right one"))
    (if (= 1 left-index)
      (query ft right-index)
      (- (query ft right-index) (query ft (dec left-index))))))
