(ns algorithm-data-structure.data-structures.min-heap-test
  (:require [algorithm-data-structure.data-structures.min-heap :as heap]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(def heap {:heap-container []})
(def heap2 {:heap-container [-21 -20 0 -8 6 20 0 300 10 20]})

(deftest create-test
  (is (= heap
         (heap/create))))

(deftest assoc-cont-test
  (testing "assoc kvs"
    (is (= {:heap-container [10 2]}
           (heap/assoc-cont heap 0 10 1 2))))
  (testing "assoc v"
    (is (= {:heap-container [20]}
           (heap/assoc-cont heap [20])))))

(deftest update-cont-test
  (is (= {:heap-container [10 2]}
         (heap/update-cont heap assoc 0 10 1 2))))

(deftest get-left-child-index-test
  (is (= 1
         (heap/get-left-child-index 0))))

(deftest get-right-child-index-test
  (is (= 2
         (heap/get-right-child-index 0))))

(deftest get-parent-index-test
  (is (= 0
         (heap/get-parent-index 1)))
  (is (= -1
         (heap/get-parent-index 0))))

(deftest has-parent-test
  (is (= false
         (heap/has-parent 0)))
  (is (= true
         (heap/has-parent 5))))

(deftest has-left-child-test
  (is (= true
         (heap/has-left-child heap2 0)))
  (is (= false
         (heap/has-left-child heap2 9))))

(deftest has-right-child-test
  (is (= true
         (heap/has-right-child heap2 0)))
  (is (= false
         (heap/has-right-child heap2 9))))

(deftest left-child-test
  (is (= -20
         (heap/left-child heap2 0)))
  (is (= -8
         (heap/left-child heap2 1))))

(deftest right-child-test
  (is (= 0
         (heap/right-child heap2 0)))
  (is (= 6
         (heap/right-child heap2 1))))

(deftest parent-test
  (is (= -21
         (heap/parent heap2 1))))

(deftest swap-test
  (is (= {:heap-container [-20 -21 0 -8 6 20 0 300 10 20]}
         (heap/swap heap2 0 1))))

(deftest peek-test
  (is (= nil
         (heap/peek heap)))
  (is (= -21
         (heap/peek heap2))))

(deftest poll-test
  (is (= [heap nil]
         (heap/poll heap)))
  (is (= [{:heap-container []} 29]
         (heap/poll {:heap-container [29]})))
  (is (= [{:heap-container [-20 -8 0 10 6 20 0 300 20]} -21]
         (heap/poll heap2))))

(deftest add-test
  (testing "test add"
    (is (= {:heap-container [10]}
           (-> heap
               (heap/add 10))))
    (is (= {:heap-container [6 10]}
           (-> heap
               (heap/add 10)
               (heap/add 6))))
    (is (= heap2
           (-> heap
               (heap/add 10)
               (heap/add 20)
               (heap/add 20)
               (heap/add 300)
               (heap/add 0)
               (heap/add -8)
               (heap/add 0)
               (heap/add -20)
               (heap/add -21)
               (heap/add 6))))))

(deftest find*-test
  (is (= [3]
         (heap/find* heap2 -8 nil)))
  (is (= [2 6]
         (heap/find* heap2 0 nil))))

(deftest remove*-test
  (is (= {:heap-container [-21 -20 10 -8 6 20 20 300]}
         (heap/remove* heap2 0 nil))))
