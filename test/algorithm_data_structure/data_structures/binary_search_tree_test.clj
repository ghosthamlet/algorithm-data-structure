(ns algorithm-data-structure.data-structures.binary-search-tree-test
  (:require [algorithm-data-structure.data-structures.binary-search-tree :as bst]
            [algorithm-data-structure.data-structures.hash-table :as ht]
            [clojure.test :refer :all]))

(def tree (bst/create))

(deftest create-test
  (is (= {:root {:left nil
                 :right nil
                 :has-parent true
                 :value nil
                 :meta (ht/create)}}
         (bst/create))))

(deftest uncle-test
  (is (= {:left nil
          :right {:left nil
                  :right nil
                  :has-parent true
                  :value 0.5
                  :meta (ht/create)}
          :has-parent true
          :value 0
          :meta (ht/create)}
         (-> tree
             (bst/insert 1)
             (bst/insert 0)
             (bst/insert 2)
             (bst/insert 3)
             (bst/insert 0.5)
             (bst/uncle {:left nil
                         :right nil
                         :has-parent true
                         :value 3
                         :meta (ht/create)})))))
