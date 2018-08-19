(ns algorithm-data-structure.algorithms.string.longest-common-substring)

(defn run [s1 s2]
  (let [len1 (count s1)
        len2 (count s2)
        tmp (repeat (count len2)
                    (repeat (count len1) nil))
        substring-matrix (->> len1 inc range (assoc tmp 0)
                              (map #(assoc % 0 0)) atom)
        longest-substring-length (atom 0)
        longest-substring-column (atom 0)
        longest-substring-row (atom 0)
        longest-substring (atom "")]
    (doseq [row-index (range 1 (inc len2))
            column-index (range 1 (inc len1))
            :let [item (get-in @substring-matrix [row-index column-index])]]
      (swap! substring-matrix assoc-in [row-index column-index]
             (if (= (nth s1 (dec column-index)) (nth s2 (dec row-index)))
               (inc (get-in @substring-matrix [(dec row-index) (dec column-index)]))
               0))
      (when (> item longest-substring-length)
        (reset! longest-substring-length item)
        (reset! longest-substring-column column-index)
        (reset! longest-substring-row row-index)))
    (if (zero? @longest-substring-length)
      ""
      (do
        (while (pos? (get-in @substring-matrix
                             [longest-substring-row longest-substring-column]))
          (swap! longest-substring #(str (nth s1 (dec longest-substring-column)) %))
          (swap! longest-substring-row dec)
          (swap! longest-substring-column dec))
        @longest-substring))))
