(ns algorithm-data-structure.data-structures.priority-queue-test
  (:require [algorithm-data-structure.data-structures.priority-queue :as queue]
            [algorithm-data-structure.comparator :refer :all]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(defn create [h p]
  (merge (queue/->PriorityQueue p)
         {:heap-container h}))

(def queue (create [] {}))

(deftest create-test
  (is (= queue
         (queue/create))))

(deftest add-test
  (is (= (create [1] {1 0})
         (queue/add queue 1)))
  (is (= (create [1 0 -2 2] {0 2 -2 10 1 0 2 5})
         (-> queue
             (queue/add 1)
             (queue/add 2 5)
             (queue/add -2 10)
             (queue/add 0 2)))))

(deftest remove*-test
  (is (= (create [] {})
         (queue/remove* queue 1 nil)))
  (is (= (create [2] {2 5})
         (-> queue
             (queue/add 1 1)
             (queue/add 2 5)
             (queue/add 10 5)
             (queue/remove* 1 nil)
             (queue/remove* 10 compare-default)))))

(deftest change-priority-test
  (is (= (create [1] {1 1})
         (queue/change-priority queue 1 1)))
  (is (= (create [2 1] {1 1 2 0})
         (-> queue
             (queue/add 1 1)
             (queue/add 2 5)
             (queue/change-priority 2 0)))))

(deftest find-by-value-test
  (is (= []
         (queue/find-by-value queue 1)))
  (is (= [1]
         (-> queue
             (queue/add 1 10)
             (queue/add 2 5)
             (queue/find-by-value 1)))))

(deftest has-value-test
  (is (= false
         (queue/has-value queue 1)))
  (is (= true
         (-> queue
             (queue/add 1 1)
             (queue/add 2 5)
             (queue/has-value 2)))))

(deftest compare-value-test
  (is (= 0
         (compare-value queue -20 0))))
