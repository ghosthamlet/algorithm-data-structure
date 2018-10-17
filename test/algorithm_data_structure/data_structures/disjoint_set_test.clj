(ns algorithm-data-structure.data-structures.disjoint-set-test
  (:require [algorithm-data-structure.data-structures.disjoint-set :as ds]
            [clojure.test :refer :all]))

(def disjoint (ds/create))

(deftest create-test
  (is (= {:key-callback nil
          :items {}}
         disjoint)))

(deftest make-set-test
  (is (= {:key-callback nil
          :items {1 {:value 1
                     :key-callback nil
                     :parent nil
                     :children {}}}}
         (-> disjoint
             (ds/make-set 1))))
  (is (= {:key-callback nil
          :items {1 {:value 1
                     :key-callback nil
                     :parent nil
                     :children {}}
                  2 {:value 2
                     :key-callback nil
                     :parent nil
                     :children {}}}}
         (-> disjoint
             (ds/make-set 1)
             (ds/make-set 1)
             (ds/make-set 2)))))

(deftest dsi-key-test
  (is (= 1
         (#'ds/dsi-key disjoint 1))
      "test private fn"))

(deftest find*-test
  (is (= nil
         (-> disjoint
             (ds/find* 1))))
  (is (= 1
       (-> disjoint
           (ds/make-set 1)
           (ds/find* 1)))))

(deftest union-test
  (is (thrown? Exception
               (-> disjoint
                   (ds/union 1 2))))
  (is (= (-> disjoint
             (ds/make-set 1))
         (-> disjoint
             (ds/make-set 1)
             (ds/union 1 1))))
  (is (= {:key-callback nil
          :items {1 {:value 1
                     :key-callback nil
                     :parent nil
                     :children {2 {:value 2
                                   :key-callback nil
                                   :parent {:value 1
                                            :key-callback nil
                                            :parent nil
                                            :children {}}
                                   :children {}}}}
                  2 {:value 2
                     :key-callback nil
                     :parent nil
                     :children {}}
                  3 {:value 3
                     :key-callback nil
                     :parent nil
                     :children {}}}}
         (-> disjoint
             (ds/make-set 1)
             (ds/make-set 2)
             (ds/make-set 3)
             (ds/union 1 2)))))

(deftest same-set?-test
  (is (thrown? Exception
               (-> disjoint
                   (ds/same-set? 1 2))))
  (is (= true
         (-> disjoint
             (ds/make-set 1)
             (ds/same-set? 1 1)))))
