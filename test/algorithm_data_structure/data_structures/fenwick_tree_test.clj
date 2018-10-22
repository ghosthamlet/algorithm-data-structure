(ns algorithm-data-structure.data-structures.fenwick-tree-test
  (:require [algorithm-data-structure.data-structures.fenwick-tree :as ft]
            [clojure.test :refer :all]))

(def tree (ft/create 5))

(deftest create-test
  (is (= {:array-size 5
          :tree-array [0 0 0  0 0 0]}
         tree)))

(deftest increase-test
  (is (thrown? Exception
               (-> tree
                   (ft/increase 0))))
  (is (thrown? Exception
               (-> tree
                   (ft/increase 6))))
  (is (= {:array-size 5
          :tree-array [0 0 10  0 10 0]}
         (-> tree
             (ft/increase 2 10)))))

(deftest query-test
  (is (thrown? Exception
               (-> tree
                   (ft/query 0))))
  (is (thrown? Exception
               (-> tree
                   (ft/query 6))))
  (is (= 15
         (-> tree
             (ft/increase 2 10)
             (ft/increase 3 5)
             (ft/query 5)))))

(deftest query-range-test
  (is (thrown? Exception
               (-> tree
                   (ft/query-range 1 0))))
  (is (= 10
         (-> tree
             (ft/increase 2 10)
             (ft/query-range 1 2))))
  (is (= 0
         (-> tree
             (ft/increase 2 10)
             (ft/increase 3 5)
             ;; (#(do (prn %) %))
             (ft/query-range 4 5)))))
