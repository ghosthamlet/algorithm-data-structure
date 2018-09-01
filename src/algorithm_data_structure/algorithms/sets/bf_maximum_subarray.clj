(ns algorithm-data-structure.algorithms.sets.bf-maximum-subarray)

(defn run [input-array]
  (let [max-subarray-start-index (atom 0)
        max-subarray-length (atom 0)
        max-subarray-sum (atom nil)
        ilen (count input-array)]
    (loop [start-index 0]
      (if (= start-index ilen)
        (take @max-subarray-length
              (drop @max-subarray-start-index input-array))
        (let [subarray-sum (atom 0)]
          (doseq [arr-length (range 1 (inc (- ilen start-index)))]
            (swap! subarray-sum + (input-array (+ start-index (dec arr-length))))
            (when (or (nil? @max-subarray-sum)
                      (> @subarray-sum @max-subarray-sum))
              (reset! max-subarray-sum @subarray-sum)
              (reset! max-subarray-start-index start-index)
              (reset! max-subarray-length arr-length)))
          (recur (inc start-index)))))))
