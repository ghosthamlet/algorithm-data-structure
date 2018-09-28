(ns algorithm-data-structure.data-structures.linked-list-test
  (:require [algorithm-data-structure.data-structures.linked-list :as ll]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(def llist (ll/create))

(deftest get-link-path-test
  (is (= [:head]
         (ll/get-link-path llist)))
  (is (= [:head :next]
         (ll/get-link-path {:head {:value 1
                                   :next nil}
                            :tail {:value 1
                                   :next nil}})))
  (is (= [:head :next :next]
         (ll/get-link-path {:head {:value 1
                                   :next {:value 2
                                          :next nil}}
                            :tail {:value 2
                                   :next nil}}))))

(deftest append-test
  (is (= {:head {:value 1
                 :next nil}
          :tail {:value 1
                 :next nil}}
         (-> llist
             (ll/append 1)
             record->map)))
  (is (= {:head {:value 1
                 :next {:value 2
                        :next nil}}
          :tail {:value 2
                 :next nil}}
         (-> llist
             (ll/append 1)
             (ll/append 2)
             record->map)))
  (is (= {:head {:value 1
                 :next {:value 2
                        :next {:value 3
                               :next nil}}}
          :tail {:value 3
                 :next nil}}
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 3)
             record->map))))

(deftest prepend-test
  (is (= {:head {:value 1
                 :next nil}
          :tail {:value 1
                 :next nil}}
         (-> llist
             (ll/prepend 1)
             record->map)))
  (is (= {:head {:value 2
                 :next {:value 1
                        :next nil}}
          :tail {:value 1
                 :next nil}}
         (-> llist
             (ll/prepend 1)
             (ll/prepend 2)
             record->map)))
  (is (= {:head {:value 3
                 :next {:value 2
                        :next {:value 1
                               :next nil}}}
          :tail {:value 1
                 :next nil}}
         (-> llist
             (ll/prepend 1)
             (ll/prepend 2)
             (ll/prepend 3)
             record->map))))

(deftest delete-test
  (is (= [llist nil]
         (ll/delete llist 1)))
  (is (= [{:head {:value 1
                  :next nil}
           :tail {:value 1
                  :next nil}}
          {:value 2
           :next nil}]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/delete 2)
             record->map)))
  (is (= [{:head {:value 2
                  :next {:value 3
                         :next nil}}
           :tail {:value 3
                  :next nil}}
          {:value 1
           :next {:value 2
                  :next {:value 3
                         :next nil}}}]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 3)
             (ll/delete 1)
             record->map)))
  (is (= [{:head {:value 2
                  :next nil}
           :tail {:value 2
                  :next nil}}
          {:value 1
           :next nil}]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 1)
             (ll/delete 1)
             record->map))))

(deftest find*-test
  (is (= nil
         (ll/find* llist 1 nil)))
  (is (= {:value 2
          :next {:value 3
                 :next nil}}
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 3)
             (ll/find* 2 nil)
             record->map))))

(deftest delete-tail-test
  (is (= [llist nil]
         (ll/delete-tail llist)))
  (is (= [{:head {:value 1
                  :next {:value 2
                         :next nil}}
           :tail {:value 2
                  :next nil}}
          {:value 3
           :next nil}]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 3)
             (ll/delete-tail)
             record->map))))

(deftest delete-head-test
  (is (= [llist nil]
         (ll/delete-head llist)))
  (is (= [{:head {:value 2
                  :next nil}
           :tail {:value 2
                  :next nil}}
          {:value 1
           :next {:value 2
                  :next nil}}]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/delete-head)
             record->map)))
  (is (= [{:head {:value 2
                  :next {:value 3
                         :next nil}}
           :tail {:value 3
                  :next nil}}
          {:value 1
           :next {:value 2
                  :next {:value 3
                         :next nil}}}]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 3)
             (ll/delete-head)
             record->map))))

(deftest ->array-test
  (is (= []
         (ll/->array llist)))
  (is (= [{:value 1
           :next {:value 2
                  :next {:value 3
                         :next nil}}}
          {:value 2
           :next {:value 3
                  :next nil}}
          {:value 3
           :next nil}]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 3)
             (ll/->array)
             record->map))))

(deftest ->tring-test
  (is (= []
         (ll/->string llist str)))
  (is (= ["1" "2" "3"]
         (-> llist
             (ll/append 1)
             (ll/append 2)
             (ll/append 3)
             (ll/->string)))))
