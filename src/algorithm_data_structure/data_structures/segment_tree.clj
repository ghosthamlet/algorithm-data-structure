(ns algorithm-data-structure.data-structures.segment-tree
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree/segment-tree"
  (:require [algorithm-data-structure.algorithms.math.is-power-of-two :as is-power-of-two]))

(declare get-left-child-index get-right-child-index
         build-tree-recursively range-query-recursive
         build-segment-tree)

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn init-segment-tree [input-array]
  (vec (repeat (dec (* 2 (let [input-array-length (count input-array)]
                       (if (is-power-of-two/run input-array-length)
                         input-array-length
                         (-> input-array-length log2 Math/floor inc (Math/pow 2))))))
           nil)))

(defn create [input-array operation operation-fallback]
  (build-segment-tree {:input-array input-array
                       :operation operation
                       :operation-fallback operation-fallback
                       :segment-tree (init-segment-tree input-array)}))

(defn build-segment-tree [self]
  (let [left-index 0
        right-index (dec (count (:input-array self)))
        position 0]
    (build-tree-recursively self left-index right-index position)))

(defn build-tree-recursively [self left-input-index right-input-index position]
  (if (= left-input-index right-input-index)
    (assoc-in self [:segment-tree position]
              (get-in self [:input-array left-input-index]))
    (let [middel-index (-> left-input-index (+ right-input-index) (/ 2) Math/floor int)
          self (-> self
                 (build-tree-recursively left-input-index
                                         middel-index (get-left-child-index self position))
                 (build-tree-recursively (inc middel-index)
                                         right-input-index (get-right-child-index self position)))]
      (assoc-in self [:segment-tree position]
                ((:operation self)
                 (get-in self [:segment-tree (get-left-child-index self position)])
                 (get-in self [:segment-tree (get-right-child-index self position)]))))))

(defn range-query [self query-left-index query-right-index]
  (let [left-index 0
        right-index (dec (count (:input-array self)))
        position 0]
    (range-query-recursive self query-left-index query-right-index
                           left-index right-index position)))

(defn range-query-recursive [self query-left-index query-right-index
                             left-index right-index position]
  (if (and (<= query-left-index left-index)
           (>= query-right-index right-index))
    (get-in self [:segment-tree position])
    (if (or (> query-left-index right-index)
            (< query-right-index left-index))
      (:operation-fallback self)
      (let [middel-index (-> left-index (+ right-index) (/ 2) Math/floor int)
            left-operation-result (range-query-recursive self query-left-index
                                                         query-right-index left-index
                                                         middel-index (get-left-child-index self position))
            right-operation-result (range-query-recursive self query-left-index
                                                          query-right-index (inc middel-index)
                                                          right-index (get-right-child-index self position))]
        ((:operation self) left-operation-result right-operation-result)))))

(defn get-left-child-index [self parent-index]
  (inc (* parent-index 2)))

(defn get-right-child-index [self parent-index]
  (+ (* parent-index 2) 2))
