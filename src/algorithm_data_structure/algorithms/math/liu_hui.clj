(ns algorithm-data-structure.algorithms.math.liu-hui)

(def circle-radius 1)

(defn get-n-gon-side-length [side-length split-counter]
  (if (<= split-counter 0)
    side-length
    (let [half-side (/ side-length 2)
          perpendicular (Math/sqrt (- (Math/pow circle-radius 2)
                                      (Math/pow half-side 2)))
          excess-radius (- circle-radius perpendicular)
          ;; split-side-length (Math/sqrt (+ (Math/pow excess-radius 2) (Math/pow half-side 2)))
          split-side-length (Math/hypot excess-radius half-side)]
      (recur split-side-length (dec split-counter)))))

(defn get-n-gon-side-count [split-count]
  (let [hexagon-sides-count 6]
    (* hexagon-sides-count
       (if (pos? split-count) (Math/pow 2 split-count) 1))))

(defn run
  ([] (run 1))
  ([split-count]
   (let [n-gon-side-length (get-n-gon-side-length circle-radius (dec split-count))
         n-gon-side-count (get-n-gon-side-count (dec split-count))
         n-gon-perimeter (* n-gon-side-length n-gon-side-count)
         approximate-circle-area (* (/ n-gon-perimeter 2) circle-radius)]
     (/ approximate-circle-area (Math/pow circle-radius 2)))))
