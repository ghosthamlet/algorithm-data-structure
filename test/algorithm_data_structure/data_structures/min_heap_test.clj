(ns algorithm-data-structure.data-structures.min-heap-test
  (:require [algorithm-data-structure.data-structures.min-heap :as mh]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(def heap {:heap-container []})
(def heap2 {:heap-container [-21 -20 0 -8 6 20 0 300 10 20]})

(deftest create-test
  (is (= heap
         (mh/create))))

(deftest assoc-cont-test
  (testing "assoc kvs"
    (is (= {:heap-container [10 2]}
           (mh/assoc-cont heap 0 10 1 2))))
  (testing "assoc v"
    (is (= {:heap-container [20]}
           (mh/assoc-cont heap [20])))))

(deftest update-cont-test
  (is (= {:heap-container [10 2]}
         (mh/update-cont heap assoc 0 10 1 2))))

(deftest get-left-child-index-test
  (is (= 1
         (mh/get-left-child-index 0))))

(deftest get-right-child-index-test
  (is (= 2
         (mh/get-right-child-index 0))))

(deftest get-parent-index-test
  (is (= 0
         (mh/get-parent-index 1)))
  (is (= -1
         (mh/get-parent-index 0))))

(deftest has-parent-test
  (is (= false
         (mh/has-parent 0)))
  (is (= true
         (mh/has-parent 5))))

(deftest has-left-child-test
  (is (= true
         (mh/has-left-child heap2 0)))
  (is (= false
         (mh/has-left-child heap2 9))))

(deftest has-right-child-test
  (is (= true
         (mh/has-right-child heap2 0)))
  (is (= false
         (mh/has-right-child heap2 9))))

(deftest left-child-test
  (is (= -20
         (mh/left-child heap2 0)))
  (is (= -8
         (mh/left-child heap2 1))))

(deftest right-child-test
  (is (= 0
         (mh/right-child heap2 0)))
  (is (= 6
         (mh/right-child heap2 1))))

(deftest parent-test
  (is (= -21
         (mh/parent heap2 1))))

(deftest swap-test
  (is (= {:heap-container [-20 -21 0 -8 6 20 0 300 10 20]}
         (mh/swap heap2 0 1))))

(deftest peek-test
  (is (= nil
         (mh/peek heap)))
  (is (= -21
         (mh/peek heap2))))

(deftest poll-test
  (is (= [heap nil]
         (mh/poll heap)))
  (is (= [{:heap-container []} 29]
         (mh/poll {:heap-container [29]})))
  (is (= [{:heap-container [-20 -8 0 10 6 20 0 300 20]} -21]
         (mh/poll heap2))))

(deftest add-test
  (testing "test add"
    (is (= {:heap-container [10]}
           (-> heap
               (mh/add 10))))
    (is (= {:heap-container [6 10]}
           (-> heap
               (mh/add 10)
               (mh/add 6))))
    (is (= heap2
           (-> heap
               (mh/add 10)
               (mh/add 20)
               (mh/add 20)
               (mh/add 300)
               (mh/add 0)
               (mh/add -8)
               (mh/add 0)
               (mh/add -20)
               (mh/add -21)
               (mh/add 6))))))

(deftest find*-test
  (is (= [3]
         (mh/find* heap2 -8 nil)))
  (is (= [2 6]
         (mh/find* heap2 0 nil))))

(deftest remove*-test
  (is (= {:heap-container [-21 -20 10 -8 6 20 20 300]}
         (mh/remove* heap2 0 nil))))
