(ns algorithm-data-structure.data-structures.disjoint-set-item-test
  (:require [algorithm-data-structure.data-structures.disjoint-set-item :as dsi]
            [clojure.test :refer :all]))

(def disjoint (dsi/create 1))

(deftest create-test
  (is (= {:value 1
          :key-callback nil
          :parent nil
          :children {}}
         disjoint)))

(deftest get-key-test
  (is (= 1
         (-> disjoint
             (dsi/get-key))))
  (is (= 2
         (-> (dsi/create {:a 2} :a)
             (dsi/get-key)))))

(deftest set-parent-test
  (is (= {:value 1
          :key-callback nil
          :parent {:value 2
                   :key-callback nil
                   :parent nil
                   :children {1 {:value 1
                                 :key-callback nil
                                 :parent {:value 2
                                          :key-callback nil
                                          :parent nil
                                          :children {}}
                                 :children {}}}}
          :children {}}
       (-> disjoint
           (dsi/set-parent (dsi/create 2))))))

(deftest add-child-test
  (is (= {:value 1
          :key-callback nil
          :parent nil
          :children {2 {:value 2
                        :key-callback nil
                        :parent {:value 1
                                 :key-callback nil
                                 :parent nil
                                 :children {}}
                        :children {}}}}
         (-> disjoint
             (dsi/add-child (dsi/create 2))))))

(deftest root?-test
  (is (= true
         (-> disjoint
             dsi/root?)))
  (is (= false
         (-> disjoint
             (dsi/set-parent (dsi/create 2))
             dsi/root?))))

(deftest get-root-test
  (is (= disjoint
         (-> disjoint
             dsi/get-root)))
  (is (= {:value 2
          :key-callback nil
          :parent nil
          :children {1 {:value 1
                        :key-callback nil
                        :parent {:value 2
                                 :key-callback nil
                                 :parent nil
                                 :children {}}
                        :children {}}}}
         (-> disjoint
             (dsi/set-parent (dsi/create 2))
             dsi/get-root))))

(deftest get-children-test
  (is (= nil
         (-> disjoint
             dsi/get-children)))
  (is (= '({:value 2
            :key-callback nil
            :parent {:value 1
                     :key-callback nil
                     :parent nil
                     :children {}}
            :children {}})
         (-> disjoint
             (dsi/add-child (dsi/create 2))
             dsi/get-children))))

(deftest get-rank-test
  (is (= 0
         (-> disjoint
             dsi/get-rank)))
  (is (= 1
         (-> disjoint
             (dsi/add-child (dsi/create 2))
             dsi/get-rank))))
