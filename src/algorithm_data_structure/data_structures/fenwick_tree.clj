(ns algorithm-data-structure.data-structures.fenwick-tree
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree/fenwick-tree")

(defn create [array-size]
  {:array-size array-size
   :tree-array (vec (repeat (inc array-size) 0))})

(defn increase [self position value]
  (if (or (< position 1) (> position (:array-size self)))
    (throw (Exception. "Position is out of allowed range"))
    (loop [i position
           self self]
      (if (> i (:array-size self))
        self
        (recur (+ i (bit-and i (- i)))
               (update-in self [:tree-array i] + value))))))

(defn query [self position]
  (if (or (< position 1) (> position (:array-size self)))
    (throw (Exception. "Position is out of allowed range"))
    (loop [i position
           sum 0]
      (if (zero? i)
        sum
        (recur (- i (bit-and i (- i)))
               (+ sum (get-in self [:tree-array i])))))))

(defn query-range [self left-index right-index]
  (if (> left-index right-index)
    (throw (Exception. "Left index can not be greater then right one"))
    (if (= 1 left-index)
      (query self right-index)
      (- (query self right-index) (query self (dec left-index))))))
