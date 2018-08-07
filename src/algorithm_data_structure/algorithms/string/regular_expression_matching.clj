(ns algorithm-data-structure.algorithms.string.regular-expression-matching)

(def zero-or-more-chars "*")
(def any-char ".")

(defn run [string pattern]
  (let [slen (count string)
        plen (count pattern)
        tmp (repeat (inc slen)
                    (repeat (inc plen) nil))
        match-matrix (atom (assoc-in tmp [0 0] true))]
    (doseq [column-index (range 1 (inc plen))]
      (swap! match-matrix assoc-in [0 column-index]
             (if (= (nth pattern (dec column-index))
                    zero-or-more-chars)
               (get-in @match-matrix [0 (- column-index 2)])
               false)))
    (doseq [row-index (range 1 (inc slen))]
      (swap! match-matrix [row-index 0] false))
    (doseq [row-index (range 1 (inc slen))]
      (doseq [column-index (range 1 (inc plen))]
        (let [string-index (dec row-index)
              pattern-index (dec column-index)]
          (swap! match-matrix assoc-in [row-index column-index]
                 (cond
                   (= (nth pattern pattern-index) zero-or-more-chars)
                   (cond
                     (get-in @match-matrix [row-index (- column-index 2)]) true
                     (and (or (= (nth pattern (dec pattern-index))
                                 (nth string string-index))
                              (= (nth pattern (dec pattern-index))
                                 any-char))
                          (get-in @match-matrix [(dec row-index) column-index])) true
                     :else false)
                   (or (= (nth pattern pattern-index)
                          (nth string string-index))
                       (= (nth pattern pattern-index)
                          any-char))
                   (get-in @match-matrix [(dec row-index) (dec column-index)])
                   :else false)))))
    (get-in @match-matrix [slen plen])))
