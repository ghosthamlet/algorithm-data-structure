(ns algorithm-data-structure.algorithms.math.trial-division)

(defn run [number]
  (cond
    (not= 0 (mod number 1)) false
    (<= number 1) false
    (<= number 3) true
    (zero? (mod number 2)) false
    :else (let [divider-limit (Math/sqrt number)]
            (loop [divider 3]
              (if (> divider divider-limit)
                true
                (if (zero? (mod number divider))
                  false
                  (recur (+ divider 2))))))))
