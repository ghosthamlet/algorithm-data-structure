(ns algorithm-data-structure.data-structures.priority-queue-test
  (:require [algorithm-data-structure.data-structures.priority-queue :as pq]
            [algorithm-data-structure.comparator :refer :all]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(defn create [h p]
  (merge (pq/->PriorityQueue p)
         {:heap-container h}))

(def queue (create [] {}))

(deftest create-test
  (is (= queue
         (pq/create))))

(deftest add-test
  (is (= (create [1] {1 0})
         (pq/add queue 1)))
  (is (= (create [1 0 -2 2] {0 2 -2 10 1 0 2 5})
         (-> queue
             (pq/add 1)
             (pq/add 2 5)
             (pq/add -2 10)
             (pq/add 0 2)))))

(deftest remove*-test
  (is (= (create [] {})
         (pq/remove* queue 1 nil)))
  (is (= (create [2] {2 5})
         (-> queue
             (pq/add 1 1)
             (pq/add 2 5)
             (pq/add 10 5)
             (pq/remove* 1 nil)
             (pq/remove* 10 compare-default)))))

(deftest change-priority-test
  (is (= (create [1] {1 1})
         (pq/change-priority queue 1 1)))
  (is (= (create [2 1] {1 1 2 0})
         (-> queue
             (pq/add 1 1)
             (pq/add 2 5)
             (pq/change-priority 2 0)))))

(deftest find-by-value-test
  (is (= []
         (pq/find-by-value queue 1)))
  (is (= [1]
         (-> queue
             (pq/add 1 10)
             (pq/add 2 5)
             (pq/find-by-value 1)))))

(deftest has-value-test
  (is (= false
         (pq/has-value queue 1)))
  (is (= true
         (-> queue
             (pq/add 1 1)
             (pq/add 2 5)
             (pq/has-value 2)))))

(deftest compare-value-test
  (is (= 0
         (compare-value queue -20 0))))
