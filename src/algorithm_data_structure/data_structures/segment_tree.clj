(ns algorithm-data-structure.data-structures.segment-tree
  (:require [algorithm-data-structure.algorithms.math.is-power-of-two :as is-power-of-two]))

(declare get-left-child-index get-right-child-index
         build-tree-recursively range-query-recursive)

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn init-segment-tree [input-array]
  (repeat (dec (* 2 (let [input-array-length (count input-array)]
                      (if (is-power-of-two/run input-array-length)
                        input-array-length
                        (-> input-array-length log2 Math/floor inc (Math/pow 2))))))
          nil))

(defn create [input-array operation operation-fallback]
  (build-segment-tree {:input-array input-array
                       :operation operation
                       :operation-fallback operation-fallback
                       :segment-tree (init-segment-tree input-array)}))

(defn build-segment-tree [st]
  (let [left-index 0
        right-index (dec (count (:input-array st)))
        position 0]
    (build-tree-recursively left-index right-index position)))

(defn build-tree-recursively [st left-input-index right-input-index position]
  (if (= left-input-index right-input-index)
    (assoc-in st [:segment-tree position]
              (get-in st [:input-array left-input-index]))
    (let [middel-index (-> left-input-index (+ right-input-index) (/ 2) Math/floor)
          st (-> st
                 (build-tree-recursively left-input-index
                                         middel-index (get-left-child-index st position))
                 (build-tree-recursively (inc middel-index)
                                         right-input-index (get-right-child-index st position)))]
      (assoc-in st [:segment-tree position]
                ((:operation st)
                 (get-in st [:segment-tree (get-left-child-index st position)])
                 (get-in st [:segment-tree (get-right-child-index st position)]))))))

(defn range-query [st query-left-index query-right-index]
  (let [left-index 0
        right-index (dec (count (:input-array st)))
        position 0]
    (range-query-recursive st query-left-index query-right-index
                           left-index right-index position)))

(defn range-query-recursive [st query-left-index query-right-index
                             left-index right-index position]
  (if (and (<= query-left-index left-index)
           (>= query-right-index right-index))
    (get-in st [:segment-tree position])
    (if (or (> query-left-index right-index)
            (< query-right-index left-index))
      (:operation-fallback st)
      (let [middel-index (Math/floor (/ (+ left-index right-index) 2))
            left-operation-result (range-query-recursive st query-left-index
                                                         query-right-index left-index
                                                         middel-index (get-left-child-index st position))
            right-operation-result (range-query-recursive st query-left-index
                                                          query-right-index (inc middel-index)
                                                          right-index (get-right-child-index st position))]
        ((:operation st) left-operation-result right-operation-result)))))

(defn get-left-child-index [st parent-index]
  (inc (* parent-index 2)))

(defn get-right-child-index [st parent-index]
  (+ (* parent-index 2) 2))
