(ns algorithm-data-structure.data-structures.segment-tree-test
  (:require [algorithm-data-structure.data-structures.segment-tree :as st]
            [clojure.test :refer :all]))

(deftest log2-test
  (is (= 0.0
         (st/log2 1)))
  (is (= 1.0
         (st/log2 2)))
  (is (= 1.5849625007211563
         (st/log2 3))))

(deftest init-segment-tree-test
  (let [arr [112 9 10 2 3 7 9 0]
        len 8]
    (is (= (vec (repeat (dec (* 2 len)) nil))
           (st/init-segment-tree arr))))
  (let [arr [112 9 10 2 3 7 9 0 10]
        len 9
        len 16]
    (is (= (vec (repeat (dec (* 2 len)) nil))
           (st/init-segment-tree arr)))))

(def input-array [112 9 10 2 3 7 9 0])
(def tree (st/create input-array + 0))

(deftest create-test
  (is (= (st/build-segment-tree {:input-array input-array
                                 :operation +
                                 :operation-fallback 0
                                 :segment-tree (st/init-segment-tree input-array)})
         tree)))

(deftest get-left-child-index-test
  (is (= 1
         (-> tree
             (st/get-left-child-index 0))))
  (is (= 3
         (-> tree
             (st/get-left-child-index 1))))
  (is (= 21
         (-> tree
             (st/get-left-child-index 10)))))

(deftest get-right-child-index-test
  (is (= 2
         (-> tree
             (st/get-right-child-index 0))))
  (is (= 4
         (-> tree
             (st/get-right-child-index 1))))
  (is (= 22
         (-> tree
             (st/get-right-child-index 10)))))

(deftest build-tree-recursively-test
  (is (= {:input-array input-array
          :operation +
          :operation-fallback 0
          :segment-tree (assoc (st/init-segment-tree input-array)
                               1 (nth input-array 0))}
         (-> {:input-array input-array
              :operation +
              :operation-fallback 0
              :segment-tree (st/init-segment-tree input-array)}
             (st/build-tree-recursively 0 0 1))))
  (let [new-segment-tree-data (assoc (assoc (st/init-segment-tree input-array)
                                            3 (nth input-array 0))
                                     4 (nth input-array 1))]
    (is (= {:input-array input-array
            :operation +
            :operation-fallback 0
            :segment-tree (assoc new-segment-tree-data
                                 1 (+ (nth new-segment-tree-data
                                           (st/get-left-child-index tree 1))
                                      (nth new-segment-tree-data
                                           (st/get-right-child-index tree 1))))}
           (-> {:input-array input-array
                :operation +
                :operation-fallback 0
                :segment-tree (st/init-segment-tree input-array)}
               (st/build-tree-recursively 0 1 1))))))

(deftest build-segment-tree-test
  (is (= (-> {:input-array input-array
              :operation +
              :operation-fallback 0
              :segment-tree (st/init-segment-tree input-array)}
             (st/build-tree-recursively 0 (dec (count input-array)) 0))
         (-> {:input-array input-array
              :operation +
              :operation-fallback 0
              :segment-tree (st/init-segment-tree input-array)}
             st/build-segment-tree))))

(deftest range-query-test
  (is (= 112
         (-> tree
             (st/range-query 0 0))))
  (is (= (+ 9 10 2)
         (-> tree
             st/build-segment-tree
             (st/range-query 1 3))))
  (is (= (+ 112 9 10 2 3 7)
         (-> tree
             st/build-segment-tree
             (st/range-query 0 5)))))
