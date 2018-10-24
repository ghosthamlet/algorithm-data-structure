(ns algorithm-data-structure.data-structures.trie-node-test
  (:require [algorithm-data-structure.data-structures.trie-node :as tn]
            [algorithm-data-structure.data-structures.hash-table :as ht]
            [clojure.test :refer :all]))

(def node (tn/create "abc"))

(deftest create-test
  (is (= {:character "abc"
          :is-complete-word false
          :children (ht/create)}
         node)))

(deftest add-child-test
  (is (= [{:character "abc"
           :is-complete-word false
           :children (-> (ht/create)
                         (ht/set* "efg" (tn/create "efg")))}
          (tn/create "efg")]
         (-> node
             (tn/add-child "efg"))))
  (is (= [{:character "abc"
           :is-complete-word false
           :children (-> (ht/create)
                         (ht/set* "efg" (tn/create "efg")))}
          (tn/create "efg")]
         (-> node
             (tn/add-child "efg")
             first
             (tn/add-child "efg")))))

(deftest get-child-test
  (is (= nil
         (-> node
             (tn/get-child "efg"))))
  (is (= (tn/create "efg")
         (-> node
             (tn/add-child "efg")
             first
             (tn/get-child "efg")))))

(deftest has-child-test
  (is (= false
         (-> node
             (tn/has-child "efg"))))
  (is (= true
         (-> node
             (tn/add-child "efg")
             first
             (tn/has-child "efg")))))

(deftest suggest-children-test
  (is (= nil
         (-> node
             tn/suggest-children)))
  (is (= ["efg" "hij"]
         (-> node
             (tn/add-child "efg")
             first
             (tn/add-child "hij")
             first
             tn/suggest-children))))

(deftest ->string-test
  (is (= "abc"
         (-> node
             tn/->string)))
  (is (= "abc:(\"efg\")"
         (-> node
             (tn/add-child "efg")
             first
             tn/->string))))
