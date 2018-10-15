(ns algorithm-data-structure.data-structures.avl-tree-test
  (:require [algorithm-data-structure.data-structures.avl-tree :as at]
            [algorithm-data-structure.data-structures.hash-table :as ht]
            [algorithm-data-structure.data-structures.binary-search-tree :as bst]
            [clojure.test :refer :all]))

(def tree (at/create))

(deftest rotate-left-left-test
  (is (= {:root {:left {:left {:left nil
                               :right nil
                               :has-parent true
                               :value 0
                               :meta (ht/create)}
                        :right {:left nil
                                :right nil
                                :has-parent true
                                :value 3
                                :meta (ht/create)}
                        :has-parent true
                        :value 2
                        :meta (ht/create)}
                 :right nil
                 :has-parent true
                 :value 5
                 :meta (ht/create)}}
         (-> tree
             (bst/insert 5)
             (bst/insert 3)
             (bst/insert 2)
             (bst/insert 0)
             (at/rotate-left-left {:left {:left {:left nil
                                                 :right nil
                                                 :has-parent true
                                                 :value 0
                                                 :meta (ht/create)}
                                          :right nil
                                          :has-parent true
                                          :value 2
                                          :meta (ht/create)}
                                   :right nil
                                   :has-parent true
                                   :value 3
                                   :meta (ht/create)}
                                  [:left]))))
  (is (= {:root {:left {:left {:left nil
                               :right nil
                               :has-parent true
                               :value 0
                               :meta (ht/create)}
                        :right nil
                        :has-parent true
                        :value 2
                        :meta (ht/create)}
                 :right {:left {:left nil
                                :right nil
                                :has-parent true
                                :value 4
                                :meta (ht/create)}
                         :right nil
                         :has-parent true
                         :value 5
                         :meta (ht/create)}
                 :has-parent true
                 :value 3
                 :meta (ht/create)}}
         (-> tree
             (bst/insert 5)
             (bst/insert 3)
             (bst/insert 4)
             (bst/insert 2)
             (bst/insert 0)
             (at/rotate-left-left {:left {:left {:left {:left nil
                                                        :right nil
                                                        :has-parent true
                                                        :value 0
                                                        :meta (ht/create)}
                                                 :right nil
                                                 :has-parent true
                                                 :value 2
                                                 :meta (ht/create)}
                                          :right {:left nil
                                                  :right nil
                                                  :has-parent true
                                                  :value 4
                                                  :meta (ht/create)}
                                          :has-parent true
                                          :value 3
                                          :meta (ht/create)}
                                   :right nil
                                   :has-parent true
                                   :value 5
                                   :meta (ht/create)}
                                  [])))))

(deftest rotate-left-right-test
  (is (= {:root {:left {:left {:left nil
                               :right nil
                               :has-parent true
                               :value 0
                               :meta (ht/create)}
                        :right {:left nil
                                :right nil
                                :has-parent true
                                :value 3
                                :meta (ht/create)}
                        :has-parent true
                        :value 2
                        :meta (ht/create)}
                 :right nil
                 :has-parent true
                 :value 5
                 :meta (ht/create)}}
         (-> tree
             (bst/insert 5)
             (bst/insert 3)
             (bst/insert 0)
             (bst/insert 2)
             (at/rotate-left-right {:left {:left nil
                                           :right {:left nil
                                                   :right nil
                                                   :has-parent true
                                                   :value 2
                                                   :meta (ht/create)}
                                           :has-parent true
                                           :value 0
                                           :meta (ht/create)}
                                    :right nil
                                    :has-parent true
                                    :value 3
                                    :meta (ht/create)}
                                   [:left])))))

(deftest rotate-right-right-test
  (is (= {:root {:left nil
                 :right {:left {:left nil
                                :right nil
                                :has-parent true
                                :value 2
                                :meta (ht/create)}
                         :right {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 5
                                 :meta (ht/create)}
                         :has-parent true
                         :value 3
                         :meta (ht/create)}
                 :has-parent true
                 :value 0
                 :meta (ht/create)}}
         (-> tree
             (bst/insert 0)
             (bst/insert 2)
             (bst/insert 3)
             (bst/insert 5)
             (at/rotate-right-right {:left nil
                                     :right {:left nil
                                             :right {:left nil
                                                     :right nil
                                                     :has-parent true
                                                     :value 5
                                                     :meta (ht/create)}
                                             :has-parent true
                                             :value 3
                                             :meta (ht/create)}
                                     :has-parent true
                                     :value 2
                                     :meta (ht/create)}
                                    [:right]))))
  (is (= {:root {:left {:left nil
                        :right {:left nil
                                :right nil
                                :has-parent true
                                :value 1
                                :meta (ht/create)}
                        :has-parent true
                        :value 0
                        :meta (ht/create)}
                 :right {:left nil
                         :right {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 5
                                 :meta (ht/create)}
                         :has-parent true
                         :value 3
                         :meta (ht/create)}
                 :has-parent true
                 :value 2
                 :meta (ht/create)}}
         (-> tree
             (bst/insert 0)
             (bst/insert 2)
             (bst/insert 1)
             (bst/insert 3)
             (bst/insert 5)
             (at/rotate-right-right {:left nil
                                     :right {:left {:left nil
                                                    :right nil
                                                    :has-parent true
                                                    :value 1
                                                    :meta (ht/create)}
                                             :right {:left nil
                                                     :right {:left nil
                                                             :right nil
                                                             :has-parent true
                                                             :value 5
                                                             :meta (ht/create)}
                                                     :has-parent true
                                                     :value 3
                                                     :meta (ht/create)}
                                             :has-parent true
                                             :value 2
                                             :meta (ht/create)}
                                     :has-parent true
                                     :value 0
                                     :meta (ht/create)}
                                    [])))))

(deftest rotate-right-left-test
  (is (= {:root {:left {:left nil
                        :right {:left nil
                                :right nil
                                :has-parent true
                                :value 0.5
                                :meta (ht/create)}
                        :has-parent true
                        :value 0
                        :meta (ht/create)}
                 :right {:left nil
                         :right {:left nil
                                 :right {:left nil
                                         :right nil
                                         :has-parent true
                                         :value 5
                                         :meta (ht/create)}
                                 :has-parent true
                                 :value 3
                                 :meta (ht/create)}
                         :has-parent true
                         :value 2
                         :meta (ht/create)}
                 :has-parent true
                 :value 1
                 :meta (ht/create)}}
         (-> tree
             (bst/insert 0)
             (bst/insert 2)
             (bst/insert 1)
             (bst/insert 0.5)
             (bst/insert 3)
             (bst/insert 5)
             (at/rotate-right-left {:left nil
                                    :right {:left {:left {:left nil
                                                          :right nil
                                                          :has-parent true
                                                          :value 0.5
                                                          :meta (ht/create)}
                                                   :right nil
                                                   :has-parent true
                                                   :value 1
                                                   :meta (ht/create)}
                                            :right {:left nil
                                                    :right {:left nil
                                                            :right nil
                                                            :has-parent true
                                                            :value 5
                                                            :meta (ht/create)}
                                                    :has-parent true
                                                    :value 3
                                                    :meta (ht/create)}
                                            :has-parent true
                                            :value 2
                                            :meta (ht/create)}
                                    :has-parent true
                                    :value 0
                                    :meta (ht/create)}
                                   [])))))

(deftest balance-test
  (is (= {:root {:left nil
                 :right {:left {:left nil
                                :right nil
                                :has-parent true
                                :value 2
                                :meta (ht/create)}
                         :right {:left nil
                                 :right nil
                                 :has-parent true
                                 :value 5
                                 :meta (ht/create)}
                         :has-parent true
                         :value 3
                         :meta (ht/create)}
                 :has-parent true
                 :value 0
                 :meta (ht/create)}}
         (-> tree
             (bst/insert 0)
             (bst/insert 2)
             (bst/insert 3)
             (bst/insert 5)
             (at/balance {:left nil
                          :right {:left nil
                                  :right {:left nil
                                          :right nil
                                          :has-parent true
                                          :value 5
                                          :meta (ht/create)}
                                  :has-parent true
                                  :value 3
                                  :meta (ht/create)}
                          :has-parent true
                          :value 2
                          :meta (ht/create)}
                         [:right])))))

(deftest insert-test
  (is (= {:root {:left nil
                 :right nil
                 :has-parent true
                 :value 1
                 :meta (ht/create)}}
         (-> tree
             (at/insert 1))))
  (is (= {:root {:left {:left {:left nil
                               :right nil
                               :has-parent true
                               :value 0
                               :meta (ht/create)}
                        :right {:left nil
                                :right nil
                                :has-parent true
                                :value 1.5
                                :meta (ht/create)}
                        :has-parent true
                        :value 1
                        :meta (ht/create)}
                 :right {:left nil
                         :right nil
                         :has-parent true
                         :value 3
                         :meta (ht/create)}
                 :has-parent true
                 :value 2
                 :meta (ht/create)}}
         (-> tree
             (at/insert 1)
             (at/insert 2)
             (at/insert 3)
             (at/insert 1.5)
             (at/insert 0)))))

(deftest remove*-test
  (is (thrown? Exception
               (-> tree
                   (at/remove* 0)))))
