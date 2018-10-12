(ns algorithm-data-structure.data-structures.binary-search-tree-node-test
  (:require [algorithm-data-structure.data-structures.binary-search-tree-node :as bstn]
            [algorithm-data-structure.data-structures.hash-table :as ht]
            [clojure.test :refer :all]))

(def node (bstn/create))

(deftest create-test
  (is (= {:left nil
          :right nil
          :has-parent true
          :value nil
          :meta (ht/create)}
         (bstn/create)))
  (is (= {:left nil
          :right nil
          :has-parent true
          :value 1
          :meta (ht/create)}
         (bstn/create 1))))

(deftest insert-test
  (is (= {:left nil
          :right nil
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (bstn/insert 1))))
  (is (= {:left nil
          :right {:left nil
                  :right nil
                  :has-parent true
                  :value 2
                  :meta (ht/create)}
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/insert 2))))
  (is (= {:left {:left nil
                 :right nil
                 :has-parent true
                 :value 0
                 :meta (ht/create)}
          :right nil
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/insert 0))))
  (is (= {:left nil
          :right {:left nil
                  :right {:left nil
                          :right nil
                          :has-parent true
                          :value 3
                          :meta (ht/create)}
                  :has-parent true
                  :value 2
                  :meta (ht/create)}
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/insert 3))))
  (is (= {:left {:left {:left nil
                        :right nil
                        :has-parent true
                        :value -1
                        :meta (ht/create)}
                 :right nil
                 :has-parent true
                 :value 0
                 :meta (ht/create)}
          :right nil
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/insert 0)
             (bstn/insert -1)))))

(deftest find*-test
  (is (= [nil []]
         (-> node
             (bstn/insert 1)
             (bstn/find* 0))))
  (is (= [{:left nil
           :right nil
           :has-parent true
           :value 1
           :meta (ht/create)}
          []]
         (-> node
             (bstn/insert 1)
             (bstn/find* 1))))
  (is (= [{:left nil
           :right nil
           :has-parent true
           :value 2
           :meta (ht/create)}
          [:right]]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/find* 2))))
  (is (= [{:left nil
           :right nil
           :has-parent true
           :value 0
           :meta (ht/create)}
          [:left]]
         (-> node
             (bstn/insert 1)
             (bstn/insert 0)
             (bstn/find* 0))))
  (is (= [{:left nil
           :right nil
           :has-parent true
           :value 3
           :meta (ht/create)}
          [:right :right]]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/insert 3)
             (bstn/find* 3))))
  (is (= [{:left nil
           :right nil
           :has-parent true
           :value -1
           :meta (ht/create)}
          [:left :left]]
         (-> node
             (bstn/insert 1)
             (bstn/insert 0)
             (bstn/insert -1)
             (bstn/find* -1)))))

(deftest contains?*-test
  (is (= false
         (-> node
             (bstn/insert 1)
             (bstn/contains?* 0))))
  (is (= true
         (-> node
             (bstn/insert 1)
             (bstn/contains?* 1)))))

(deftest find-min-test
  (is (= {:left nil
          :right nil
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/find-min))))
  (is (= {:left nil
          :right {:left nil
                  :right nil
                  :has-parent true
                  :value 2
                  :meta (ht/create)}
          :has-parent true
          :value 1
          :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/find-min))))
  (is (= {:left nil
          :right nil
          :has-parent true
          :value 0
          :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/insert 0)
             (bstn/find-min))))
  (is (= {:left nil
           :right nil
           :has-parent true
           :value -1
           :meta (ht/create)}
         (-> node
             (bstn/insert 1)
             (bstn/insert 0)
             (bstn/insert -1)
             (bstn/find-min)))))

(deftest remove*-test
  (is (thrown? Exception
              (-> node
                  (bstn/remove* 0))))
  (is (= [{:left nil
           :right nil
           :has-parent true
           :value nil
           :meta (ht/create)}
          true]
         (-> node
             (bstn/insert 1)
             (bstn/remove* 1))))
  (is (= [{:left nil
           :right nil
           :has-parent true
           :value 1
           :meta (ht/create)}
          true]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/remove* 2))))
  (is (= [{:left nil
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 2
                   :meta (ht/create)}
           :has-parent true
           :value 1
           :meta (ht/create)}
          true]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/insert 3)
             (bstn/remove* 3))))
  (is (= [{:left nil
           :right {:left {:left nil
                          :right nil
                          :has-parent true
                          :value 1.5
                          :meta (ht/create)}
                   :right {:left nil
                           :right nil
                           :has-parent true
                           :value 3
                           :meta (ht/create)}
                   :has-parent true
                   :value 2.5
                   :meta (ht/create)}
           :has-parent true
           :value 1
           :meta (ht/create)}
          true]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/insert 3)
             (bstn/insert 2.5)
             (bstn/insert 1.5)
             (bstn/remove* 2)))
      "node-to-remove have both left right child, next-bigger-node is not right child")
  (is (= [{:left nil
           :right {:left {:left nil
                          :right nil
                          :has-parent true
                          :value 1.5
                          :meta (ht/create)}
                   :right {:left nil
                           :right nil
                           :has-parent true
                           :value 4
                           :meta (ht/create)}
                   :has-parent true
                   :value 3
                   :meta (ht/create)}
           :has-parent true
           :value 1
           :meta (ht/create)}
          true]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/insert 3)
             (bstn/insert 4)
             (bstn/insert 1.5)
             (bstn/remove* 2)))
      "node-to-remove have both left right child, next-bigger-node is right child")
  (is (= [{:left nil
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 3
                   :meta (ht/create)}
           :has-parent true
           :value 1
           :meta (ht/create)}
          true]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/insert 3)
             (bstn/remove* 2)))
      "node-to-remove has right child and parent")
  (is (= [{:left nil
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 3
                   :meta (ht/create)}
           :has-parent true
           :value 2
           :meta (ht/create)}
          true]
         (-> node
             (bstn/insert 1)
             (bstn/insert 2)
             (bstn/insert 3)
             (bstn/remove* 1)))
      "node-to-remove has right child and no parent"))
