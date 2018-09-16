(ns algorithm-data-structure.algorithms.uncategorized.queen-position)

(defn create [row-index column-index]
  {:row-index row-index
   :column-index column-index})

(defn get-left-diagonal [qp]
  (- (:row-index qp) (:column-index qp)))

(defn get-right-diagonal [qp]
  (+ (:row-index qp) (:column-index qp)))

(defn ->string [qp]
  (str (:row-index qp) "," (:column-index qp)))
