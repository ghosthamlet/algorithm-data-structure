(ns algorithm-data-structure.algorithms.math.fibonacci)

(defn run [number-position]
  (bigint (cond
            (< number-position 1) 0
            (= number-position 1) 1
            :else (loop [iterations-counter (dec number-position)
                         fib-prev (bigint 1)
                         fib-prev-prev (bigint 0)]
                    (if (zero? iterations-counter)
                      fib-prev
                      (recur (dec iterations-counter)
                             (+ fib-prev fib-prev-prev)
                             fib-prev))))))
