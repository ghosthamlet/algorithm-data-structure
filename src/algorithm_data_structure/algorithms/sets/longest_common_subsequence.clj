(ns algorithm-data-structure.algorithms.sets.longest-common-subsequence)

(defn run [set1 set2]
  (let [len1 (count set1)
        len2 (count set2)
        lcs-matrix (repeat (inc len2)
                           (repeat (inc len1) nil))
        lcs-matrix (assoc lcs-matrix
                          0 (repeat (inc len1) 0))
        lcs-matrix (map #(assoc % 0 0) lcs-matrix)
        lcs-matrix (map-indexed (fn [row-index row]
                                  (if (= 0 row-index)
                                    row
                                    (map-indexed (fn [column-index col]
                                                   (if (= 0 col)
                                                     col
                                                     (if (= (set1 (dec column-index))
                                                            (set2 (dec row-index)))
                                                       (inc (get-in lcs-matrix
                                                                    [(dec row-index) (dec column-index)]))
                                                       (max (get-in lcs-matrix
                                                                    [(dec row-index) column-index])
                                                            (get-in lcs-matrix
                                                                    [row-index (dec column-index)])))))
                                                 row)))
                                lcs-matrix)]
    (if-not (get-in lcs-matrix [len2 len1])
      ['']
      (loop [column-index len1
             row-index len2
             longest-sequence []]
        (if (and (<= column-index 0) (<= row-index 0))
          longest-sequence
          (let [[column-index row-index longest-sequence]
                (cond
                  (= (set1 (dec column-index))
                     (set2 (dec row-index)))
                  [(dec column-index)
                   (dec row-index)
                   (concat [(set1 (dec column-index))]
                           longest-sequence)]
                  (= (get-in lcs-matrix [row-index column-index])
                     (get-in lcs-matrix [row-index (dec column-index)]))
                  [(dec column-index)
                   row-index
                   longest-sequence]
                  :else
                  [column-index (dec row-index) longest-sequence])]))))))
