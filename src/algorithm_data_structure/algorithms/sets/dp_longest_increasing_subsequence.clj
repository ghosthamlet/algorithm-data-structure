(ns algorithm-data-structure.algorithms.sets.dp-longest-increasing-subsequence)

(defn run [sequence]
  (apply max
         (loop [previous-element-index 0
                current-element-index 1
                lengths-array (repeat (count sequence) 1)]
           (if (= current-element-index (count sequence))
             lengths-array
             (let [lengths-array (if (< (sequence previous-element-index)
                                        (sequence current-element-index))
                                   (let [new-length (inc (lengths-array previous-element-index))]
                                     (if (> new-length (lengths-array current-element-index))
                                       (assoc lengths-array current-element-index new-length)
                                       lengths-array))
                                   lengths-array)
                   previous-element-index (inc previous-element-index)
                   [previous-element-index current-element-index]
                   (if (= previous-element-index current-element-index)
                     [0 (inc current-element-index)]
                     [previous-element-index current-element-index])]
               (recur previous-element-index
                      current-element-index
                      lengths-array))))))
