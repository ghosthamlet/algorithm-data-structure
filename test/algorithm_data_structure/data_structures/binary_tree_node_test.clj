(ns algorithm-data-structure.data-structures.binary-tree-node-test
  (:require [algorithm-data-structure.data-structures.binary-tree-node :as btn]
            [algorithm-data-structure.data-structures.hash-table :as ht]
            [clojure.test :refer :all]))

(def node (btn/create))

(deftest create-test
  (is (= {:left nil
          :right nil
          :has-parent true
          :value nil
          :meta (ht/create)}
         (btn/create)))
  (is (= {:left nil
          :right nil
          :has-parent true
          :value 1
          :meta (ht/create)}
         (btn/create 1))))

(deftest set-left-test
  (is (= {:left (btn/create 0)
          :right nil
          :has-parent true
          :value nil
          :meta (ht/create)}
         (-> node
             (btn/set-left (btn/create 0)))))
  (is (= {:left {:left (btn/create 0)
                 :right nil
                 :has-parent true
                 :value 1
                 :meta (ht/create)}
          :right nil
          :has-parent true
          :value nil
          :meta (ht/create)}
         (-> node
             (btn/set-left (btn/create 0))
             (btn/set-left (btn/set-left (btn/create 1) (btn/create 0)))))))

(deftest set-right-test
  (is (= {:left (btn/create 0)
          :right (btn/create 0)
          :has-parent true
          :value nil
          :meta (ht/create)}
         (-> node
             (btn/set-left (btn/create 0))
             (btn/set-right (btn/create 0)))))
  (is (= {:left {:left (btn/create 0)
                 :right nil
                 :has-parent true
                 :value 1
                 :meta (ht/create)}
          :right {:left nil
                  :right (btn/create 0)
                  :has-parent true
                  :value 1
                  :meta (ht/create)}
          :has-parent true
          :value nil
          :meta (ht/create)}
         (-> node
             (btn/set-left (btn/create 0))
             (btn/set-left (btn/set-left (btn/create 1) (btn/create 0)))
             (btn/set-right (btn/create 0))
             (btn/set-right (btn/set-right (btn/create 1) (btn/create 0)))))))

(deftest left-height-test
  (is (= 0
         (btn/left-height node)))
  (is (= 2
         (-> node
             (btn/set-left (btn/create 0))
             (btn/set-left (btn/set-left (btn/create 1) (btn/create 0)))
             btn/left-height))))

(deftest right-height-test
  (is (= 0
         (btn/right-height node)))
  (is (= 2
         (-> node
             (btn/set-right (btn/create 0))
             (btn/set-right (btn/set-right (btn/create 1) (btn/create 0)))
             btn/right-height))))

(deftest height-test
  (is (= 0
         (btn/height node)))
  (is (= 3
         (-> node
             (btn/set-left (btn/create 0))
             (btn/set-left (btn/set-left (btn/create 1) (btn/create 0)))
             (btn/set-right (btn/create 0))
             (btn/set-right (btn/set-right (btn/create 1) (btn/create 0)))
             (btn/set-right (btn/set-right (btn/create 1)
                                           (btn/set-right (btn/create 2) (btn/create 0))))
             btn/height))))

(deftest set-value-test
  (is (= {:left (btn/create 0)
          :right nil
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (btn/set-value 1)
             (btn/set-left (btn/create 0)))))
  (is (= {:left (btn/create 0)
          :right nil
          :has-parent true
          :value 2
          :meta (ht/create)}
         (-> node
             (btn/set-value 1)
             (btn/set-left (btn/create 0))
             (btn/set-value 2)))))

(deftest balance-factor-test
  (is (= 0
         (btn/balance-factor node)))
  (is (= -1
         (-> node
             (btn/set-left (btn/create 0))
             (btn/set-left (btn/set-left (btn/create 1) (btn/create 0)))
             (btn/set-right (btn/create 0))
             (btn/set-right (btn/set-right (btn/create 1) (btn/create 0)))
             (btn/set-right (btn/set-right (btn/create 1)
                                           (btn/set-right (btn/create 2) (btn/create 0))))
             btn/balance-factor))))

(deftest remove-child-test
  (is (= [node false]
         (btn/remove-child node (btn/create))))
  (is (= [(-> node
              (btn/set-left (btn/create 0)))
          false]
         (-> node
             (btn/set-left (btn/create 0))
             (btn/remove-child (btn/create)))))
  (is (= [node true]
         (-> node
             (btn/set-left (btn/create 0))
             (btn/remove-child (btn/create 0)))))
  (is (= [node true]
         (-> node
             (btn/set-right (btn/create 0))
             (btn/remove-child (btn/create 0)))))
  (is (= [(-> node
              (btn/set-left (btn/set-left (btn/create 1) (btn/create 0))))
          false]
         (-> node
             (btn/set-left (btn/set-left (btn/create 1) (btn/create 0)))
             (btn/remove-child (btn/create 1)))))
  (is (= [node true]
         (-> node
             (btn/set-left (btn/set-left (btn/create 1) (btn/create 0)))
             (btn/remove-child (btn/set-left (btn/create 1) (btn/create 0)))))))

(deftest replace-child-test
  (is (= [node false]
         (btn/replace-child node nil (btn/create))))
  (is (= [node false]
         (btn/replace-child node (btn/create) (btn/create))))
  (is (= [(-> node
              (btn/set-left (btn/create 0)))
          false]
         (-> node
             (btn/set-left (btn/create 0))
             (btn/replace-child (btn/create 2) (btn/create 1)))))
  (is (= [(-> node
              (btn/set-left (btn/create 1)))
          true]
         (-> node
             (btn/set-left (btn/create 0))
             (btn/replace-child (btn/create 0) (btn/create 1)))))
  (is (= [(-> node
              (btn/set-right (btn/create 1)))
          true]
         (-> node
             (btn/set-right (btn/create 0))
             (btn/replace-child (btn/create 0) (btn/create 1))))))

(deftest copy-node-test
  (is (= (btn/create 1)
         (btn/copy-node (btn/create 1) node)))
  (is (= (-> (btn/create 1)
             (btn/set-right (btn/create 0)))
         (btn/copy-node (-> (btn/create 1)
                            (btn/set-right (btn/create 0)))
                        node))))

(deftest traverse-in-order-test
  (is (= [nil]
         (btn/traverse-in-order node)))
  (is (= [1 0 -1 0 1]
         (-> node
             (btn/set-value -1)
             (btn/set-left (btn/create 1))
             (btn/set-right (btn/create 0))
             (btn/set-left (btn/set-right (btn/create 1) (btn/create 0)))
             (btn/set-right (btn/set-right (btn/create 0) (btn/create 1)))
             btn/traverse-in-order))))

(deftest ->string-test
  (is (= "[nil]"
         (btn/->string node)))
  (is (= "[1 0 -1 0 1]"
         (-> node
             (btn/set-value -1)
             (btn/set-left (btn/create 1))
             (btn/set-right (btn/create 0))
             (btn/set-left (btn/set-right (btn/create 1) (btn/create 0)))
             (btn/set-right (btn/set-right (btn/create 0) (btn/create 1)))
             btn/->string))))
