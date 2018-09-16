(ns algorithm-data-structure.algorithms.uncategorized.n-queens
  (:require [algorithm-data-structure.algorithms.uncategorized.queen-position :as qp]))

(defn safe? [queens-positions row-index column-index]
  (let [new-queens-positions (qp/create row-index column-index)]
    (not (some #(and %
                     (or (= (:column-index new-queens-positions)
                            (:column-index %))
                         (= (:row-index new-queens-positions)
                            (:row-index %))
                         (= (qp/get-left-diagonal new-queens-positions)
                            (qp/get-left-diagonal %))
                         (= (qp/get-right-diagonal new-queens-positions)
                            (qp/get-right-diagonal %))))
               queens-positions))))

(defn n-queens-recursive
  [solutions previous-queens-positions queens-count row-index]
  (let [queens-positions previous-queens-positions]
    (if (= row-index queens-count)
      (conj solutions queens-positions)
      (loop [column-index 0
             solutions solutions
             queens-positions queens-positions]
        (if (= column-index queens-count)
          solutions
          (if (safe? queens-positions row-index column-index)
            (let [queens-positions (assoc queens-positions
                                          row-index (qp/create row-index column-index))
                  solutions (n-queens-recursive solutions
                                                queens-positions queens-count (inc row-index))
                  queens-positions (assoc queens-positions
                                          row-index nil)]
              (recur (inc column-index)
                     solutions
                     queens-positions))
            (recur (inc column-index)
                   solutions
                   queens-positions)))))))

(defn run [queens-count]
  (n-queens-recursive [] (repeat queens-count nil) queens-count 0))

