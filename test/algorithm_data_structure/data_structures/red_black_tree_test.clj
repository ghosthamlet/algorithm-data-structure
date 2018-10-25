(ns algorithm-data-structure.data-structures.red-black-tree-test
  (:require [algorithm-data-structure.data-structures.red-black-tree :as rbt]
            [algorithm-data-structure.data-structures.binary-search-tree :as bst]
            [algorithm-data-structure.data-structures.binary-search-tree-node :as bstn]
            [algorithm-data-structure.data-structures.hash-table :as ht]
            [clojure.test :refer :all]))

(def tree (rbt/create))

(deftest create
  (is (= (bst/create)
         tree)))

(deftest remove*-test
  (is (thrown? Exception
               (-> tree
                   (rbt/remove* 0)))))

(deftest make-node-red-test
  (is (= (assoc tree
                :root (update (bstn/create)
                              :meta ht/set* rbt/color-prop-name (:red rbt/red-black-tree-colors)))
         (-> tree
             (rbt/make-node-red (bstn/create))))))

(deftest make-node-black-test
  (is (= (assoc tree
                :root (update (bstn/create)
                              :meta ht/set* rbt/color-prop-name (:black rbt/red-black-tree-colors)))
         (-> tree
             (rbt/make-node-black (bstn/create))))))

(deftest node-red?-test
  (is (= false
         (-> tree
             (rbt/node-red? (bstn/create)))))
  (is (= true
         (-> tree
             (rbt/make-node-red (bstn/create))
             (rbt/node-red? (update (bstn/create)
                                    :meta ht/set* rbt/color-prop-name (:red rbt/red-black-tree-colors)))))))

(deftest node-black?-test
  (is (= false
         (-> tree
             (rbt/node-black? (bstn/create)))))
  (is (= true
         (-> tree
             (rbt/make-node-black (bstn/create))
             (rbt/node-black? (update (bstn/create)
                                    :meta ht/set* rbt/color-prop-name (:black rbt/red-black-tree-colors)))))))

(deftest node-colored?
  (is (= false
         (-> tree
             (rbt/node-colored? (bstn/create)))))
  (is (= true
         (-> tree
             (rbt/make-node-black (bstn/create))
             (rbt/node-colored? (update (bstn/create)
                                        :meta ht/set* rbt/color-prop-name (:black rbt/red-black-tree-colors)))))))

(deftest swap-node-colors-test
  (is (= (assoc tree
                :root (update (bstn/create)
                              :meta ht/set* rbt/color-prop-name (:red rbt/red-black-tree-colors)))
         (-> tree
             (rbt/swap-node-colors (update (bstn/create)
                                           :meta ht/set* rbt/color-prop-name (:red rbt/red-black-tree-colors))
                                   (update (bstn/create)
                                           :meta ht/set* rbt/color-prop-name (:black rbt/red-black-tree-colors))))))
  (is (= (assoc tree
                :root (update (bstn/create)
                              :meta ht/set* rbt/color-prop-name (:black rbt/red-black-tree-colors)))
         (-> tree
             (rbt/swap-node-colors (update (bstn/create)
                                           :meta ht/set* rbt/color-prop-name (:black rbt/red-black-tree-colors))
                                   (update (bstn/create)
                                           :meta ht/set* rbt/color-prop-name (:red rbt/red-black-tree-colors)))))))

(deftest left-left-rotation-test
  (is (= [{:root {:left {:left nil
                         :right nil
                         :has-parent true
                         :value 0
                         :meta (ht/create)}
                  :right {:left nil
                          :right nil
                          :has-parent true
                          :value 2
                          :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                  :has-parent false
                  :value 1
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
          {:left {:left nil
                  :right nil
                  :has-parent true
                  :value 0
                  :meta (ht/create)}
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 2
                   :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
           :has-parent false
           :value 1
           :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}]
         (-> tree
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 1)
             (rbt/make-node-red (bstn/create 1))
             (bst/insert 0)
             (rbt/left-left-rotation (-> tree
                                         (bst/insert 2)
                                         (rbt/make-node-black (bstn/create 2))
                                         (bst/insert 1)
                                         (rbt/make-node-red (bstn/create 1))
                                         (bst/insert 0)
                                         :root)))))
  (is (= [{:root {:left {:left {:left nil
                                :right nil
                                :has-parent true
                                :value 0
                                :meta (ht/create)}
                         :right {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 2
                                 :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                         :has-parent true
                         :value 1
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                  :right nil
                  :has-parent true
                  :value 3
                  :meta (ht/create)}}
          {:left {:left nil
                  :right nil
                  :has-parent true
                  :value 0
                  :meta (ht/create)}
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 2
                   :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
           :has-parent true
           :value 1
           :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}]
         (-> tree
             (bst/insert 3)
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 1)
             (rbt/make-node-red (bstn/create 1))
             (bst/insert 0)
             (rbt/left-left-rotation (-> tree
                                         (bst/insert 2)
                                         (rbt/make-node-black (bstn/create 2))
                                         (bst/insert 1)
                                         (rbt/make-node-red (bstn/create 1))
                                         (bst/insert 0)
                                         :root))))))

(deftest left-right-rotation-test
  (is (= [{:root {:left {:left nil
                         :right nil
                         :has-parent true
                         :value 1
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                  :right {:left nil
                          :right nil
                          :has-parent true
                          :value 2
                          :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                  :has-parent false
                  :value 1.5
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
          {:left {:left nil
                  :right nil
                  :has-parent true
                  :value 1
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 2
                   :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
           :has-parent false
           :value 1.5
           :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}]
         (-> tree
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 1)
             (rbt/make-node-red (bstn/create 1))
             (bst/insert 1.5)
             (rbt/make-node-red (bstn/create 1.5))
             (rbt/left-right-rotation (-> tree
                                         (bst/insert 2)
                                         (rbt/make-node-black (bstn/create 2))
                                         (bst/insert 1)
                                         (rbt/make-node-red (bstn/create 1))
                                         (bst/insert 1.5)
                                         (rbt/make-node-red (bstn/create 1.5))
                                         :root))))))

(deftest right-right-rotation-test
  (is (= [{:root {:left {:left nil
                         :right nil
                         :has-parent true
                         :value 2
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                  :right {:left nil
                          :right nil
                          :has-parent true
                          :value 4
                          :meta (ht/create)}
                  :has-parent false
                  :value 3
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
          {:left {:left nil
                  :right nil
                  :has-parent true
                  :value 2
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 4
                   :meta (ht/create)}
           :has-parent false
           :value 3
           :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}]
         (-> tree
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 3)
             (rbt/make-node-red (bstn/create 3))
             (bst/insert 4)
             (rbt/right-right-rotation (-> tree
                                           (bst/insert 2)
                                           (rbt/make-node-black (bstn/create 2))
                                           (bst/insert 3)
                                           (rbt/make-node-red (bstn/create 3))
                                           (bst/insert 4)
                                           :root)))))
  (is (= [{:root {:left nil
                  :right {:left {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 3
                                 :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                          :right {:left nil
                                  :right nil
                                  :has-parent true
                                  :value 5
                                  :meta (ht/create)}
                          :has-parent true
                          :value 4
                          :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                  :has-parent true
                  :value 2
                  :meta (ht/create)}}
          {:left {:left nil
                  :right nil
                  :has-parent true
                  :value 3
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 5
                   :meta (ht/create)}
           :has-parent true
           :value 4
           :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}]
         (-> tree
             (bst/insert 2)
             (bst/insert 3)
             (rbt/make-node-black (bstn/create 3))
             (bst/insert 4)
             (rbt/make-node-red (bstn/create 4))
             (bst/insert 5)
             (rbt/right-right-rotation (-> tree
                                         (bst/insert 3)
                                         (rbt/make-node-black (bstn/create 3))
                                         (bst/insert 4)
                                         (rbt/make-node-red (bstn/create 4))
                                         (bst/insert 5)
                                         :root))))))

(deftest right-left-rotation-test
  (is (= [{:root {:left {:left nil
                         :right nil
                         :has-parent true
                         :value 2
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                  :right {:left nil
                          :right nil
                          :has-parent true
                          :value 3
                          :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                  :has-parent false
                  :value 2.5
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
          {:left {:left nil
                  :right nil
                  :has-parent true
                  :value 2
                  :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
           :right {:left nil
                   :right nil
                   :has-parent true
                   :value 3
                   :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
           :has-parent false
           :value 2.5
           :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}]
         (-> tree
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 3)
             (rbt/make-node-red (bstn/create 3))
             (bst/insert 2.5)
             (rbt/make-node-red (bstn/create 2.5))
             (rbt/right-left-rotation (-> tree
                                           (bst/insert 2)
                                           (rbt/make-node-black (bstn/create 2))
                                           (bst/insert 3)
                                           (rbt/make-node-red (bstn/create 3))
                                           (bst/insert 2.5)
                                           (rbt/make-node-red (bstn/create 2.5))
                                           :root))))))

(deftest balance-test
  (is (= {:root {:left nil
                 :right nil
                 :has-parent true
                 :value nil
                 :meta (ht/create)}}
         (-> tree
             (rbt/balance {:left nil
                           :right nil
                           :has-parent true
                           :value nil
                           :meta (ht/create)}))))
  (let [tree {:root {:left {:left nil
                            :right nil
                            :has-parent true
                            :value 0
                            :meta (ht/create)}
                     :right nil
                     :has-parent true
                     :value 1
                     :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}]
    (is (= tree
           (-> tree
               (rbt/balance {:left nil
                             :right nil
                             :has-parent true
                             :value 0
                             :meta (ht/create)})))))
  (is (= {:root {:left {:left nil
                        :right nil
                        :has-parent true
                        :value 1
                        :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                 :right {:left nil
                         :right {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 4
                                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                         :has-parent true
                         :value 3
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                 :has-parent true
                 :value 2
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 3)
             (rbt/make-node-red (bstn/create 3))
             (bst/insert 4)
             (rbt/make-node-black (bstn/create 4))
             (bst/insert 1)
             (rbt/make-node-red (bstn/create 1))
             (rbt/balance (-> tree
                              (bst/insert 4)
                              (rbt/make-node-black (bstn/create 4))
                              :root))))
      "Has uncle and uncle is red, grand-parent-node is root")

  ;; FIXME: swap color has bug
  (is (= {:root {:left nil
                 :right {:left {:left nil
                                :right nil
                                :has-parent true
                                :value 1
                                :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                         :right {:left nil
                                 :right {:left nil
                                         :right nil
                                         :has-parent true
                                         :value 4
                                         :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                                 :has-parent true
                                 :value 3
                                 :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                         :has-parent true
                         :value 2
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                 :has-parent true
                 :value 0
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (bst/insert 0)
             (rbt/make-node-black (bstn/create 0))
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 3)
             (rbt/make-node-red (bstn/create 3))
             (bst/insert 4)
             (rbt/make-node-black (bstn/create 4))
             (bst/insert 1)
             (rbt/make-node-red (bstn/create 1))
             (rbt/balance (-> tree
                              (bst/insert 4)
                              (rbt/make-node-black (bstn/create 4))
                              :root))))
      "Has uncle and uncle is red, grand-parent-node is not root")

  ;; FIXME: swap color has bug
  (is (= {:root {:left {:left {:left nil
                               :right nil
                               :has-parent true
                               :value 1
                               :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                        :right nil
                        :has-parent true
                        :value 2
                        :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                 :right {:left nil
                         :right nil
                         :has-parent true
                         :value 4
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                 :has-parent false
                 :value 3
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 3)
             (rbt/make-node-red (bstn/create 3))
             (bst/insert 4)
             (rbt/make-node-black (bstn/create 4))
             (bst/insert 1)
             (rbt/make-node-black (bstn/create 1))
             (rbt/balance (-> tree
                              (bst/insert 4)
                              (rbt/make-node-black (bstn/create 4))
                              :root))))
      "No uncle or uncle is black, grand-parent-node right node is parent-node,
      parent-node right node is node")

  ;; FIXME: swap color has bug
  (is (= {:root {:left {:left {:left nil
                               :right nil
                               :has-parent true
                               :value 1
                               :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                        :right nil
                        :has-parent true
                        :value 2
                        :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                 :right {:left nil
                         :right nil
                         :has-parent true
                         :value 3
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                 :has-parent false
                 :value 2.5
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (bst/insert 2)
             (rbt/make-node-black (bstn/create 2))
             (bst/insert 3)
             (rbt/make-node-red (bstn/create 3))
             (bst/insert 2.5)
             (rbt/make-node-black (bstn/create 2.5))
             (bst/insert 1)
             (rbt/make-node-black (bstn/create 1))
             (rbt/balance (-> tree
                              (bst/insert 2.5)
                              (rbt/make-node-black (bstn/create 2.5))
                              :root))))
      "No uncle or uncle is black, grand-parent-node right node is parent-node,
      parent-node left node is node")

  ;; FIXME: swap color has bug
  (is (= {:root {:left {:left nil
                        :right nil
                        :has-parent true
                        :value 1
                        :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                 :right {:left nil
                         :right {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 3.5
                                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                         :has-parent true
                         :value 3
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                 :has-parent false
                 :value 2
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (bst/insert 3)
             (rbt/make-node-black (bstn/create 3))
             (bst/insert 2)
             (rbt/make-node-red (bstn/create 2))
             (bst/insert 1)
             (rbt/make-node-black (bstn/create 1))
             (bst/insert 3.5)
             (rbt/make-node-black (bstn/create 3.5))
             (rbt/balance (-> tree
                              (bst/insert 1)
                              (rbt/make-node-black (bstn/create 1))
                              :root))))
      "No uncle or uncle is black, grand-parent-node left node is parent-node,
      parent-node left node is node")

  ;; FIXME: swap color has bug
  (is (= {:root {:left {:left nil
                        :right nil
                        :has-parent true
                        :value 2
                        :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                 :right {:left nil
                         :right {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 3.5
                                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                         :has-parent true
                         :value 3
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}
                 :has-parent false
                 :value 2.5
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (bst/insert 3)
             (rbt/make-node-black (bstn/create 3))
             (bst/insert 2)
             (rbt/make-node-red (bstn/create 2))
             (bst/insert 2.5)
             (rbt/make-node-black (bstn/create 2.5))
             (bst/insert 3.5)
             (rbt/make-node-black (bstn/create 3.5))
             (rbt/balance (-> tree
                              (bst/insert 2.5)
                              (rbt/make-node-black (bstn/create 2.5))
                              :root))))
      "No uncle or uncle is black, grand-parent-node left node is parent-node,
      parent-node right node is node"))

(deftest insert-test
  (is (= {:root {:left nil
                 :right nil
                 :has-parent true
                 :value 1
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (rbt/insert 1))))
  (is (= {:root {:left nil
                 :right {:left nil
                         :right nil
                         :has-parent true
                         :value 2
                         :meta (ht/set* (ht/create) rbt/color-prop-name (:red rbt/red-black-tree-colors))}
                 :has-parent true
                 :value 1
                 :meta (ht/set* (ht/create) rbt/color-prop-name (:black rbt/red-black-tree-colors))}}
         (-> tree
             (rbt/insert 1)
             (rbt/insert 2)))))
