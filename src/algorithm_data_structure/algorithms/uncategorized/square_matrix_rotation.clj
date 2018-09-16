(ns algorithm-data-structure.algorithms.uncategorized.square-matrix-rotation)

(defn- swap [xs x y]
  (assoc-in (assoc-in xs
                      x (get-in xs y))
            y (get-in xs x)))

(defn run [original-matrix]
  (let [matrix original-matrix
        mlen (count matrix)
        matrix (loop [row-idx 0
                      matrix matrix]
                 (if (= row-idx mlen)
                   matrix
                   (recur (inc row-idx)
                          (loop [col-idx (inc row-idx)
                                 matrix matrix]
                            (if (= col-idx mlen)
                              matrix
                              (recur (inc col-idx)
                                     (swap matrix
                                           [col-idx row-idx]
                                           [row-idx col-idx])))))))]
    (loop [row-idx 0
           matrix matrix]
      (if (= row-idx mlen)
        matrix
        (recur (inc row-idx)
               (loop [col-idx 0
                      matrix matrix]
                 (if (>= col-idx (/ mlen 2))
                   matrix
                   (recur (inc col-idx)
                          (swap matrix
                                [row-idx (- mlen col-idx 1)]
                                [row-idx col-idx])))))))))
