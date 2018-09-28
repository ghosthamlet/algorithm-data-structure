(ns algorithm-data-structure.data-structures.hash-table-test
  (:require [algorithm-data-structure.data-structures.hash-table :as ht]
            [algorithm-data-structure.util :refer [record->map]]
            [clojure.test :refer :all]))

(def table (ht/create 3))

(deftest create-test
  (is (= {:keys {}
          :buckets [{:head nil :tail nil}
                    {:head nil :tail nil}
                    {:head nil :tail nil}]}
         (record->map table))))

(deftest hash-test
  (is (= 0
         (ht/hash table "abc")))
  (is (= 1
         (ht/hash table "test"))))

(deftest set*-test
  (is (= {:keys {"abc" 0}
          :buckets [{:head {:value {:key "abc" :value 10} :next nil}
                     :tail {:value {:key "abc" :value 10} :next nil}}
                    {:head nil :tail nil}
                    {:head nil :tail nil}]}
         (-> table
             (ht/set* "abc" 10)
             record->map)))
  (is (= {:keys {"test" 1}
          :buckets [{:head nil :tail nil}
                    {:head {:value {:key "test" :value "a"} :next nil}
                     :tail {:value {:key "test" :value "a"} :next nil}}
                    {:head nil :tail nil}]}
         (-> table
             (ht/set* "test" "a")
             record->map)))
  (is (= {:keys {"test" 1 "abc" 0 "efg" 0}
          :buckets [{:head {:value {:key "abc" :value 10}
                            :next {:value {:key "efg" :value 'v} :next nil}}
                     :tail {:value {:key "efg" :value 'v} :next nil}}
                    {:head {:value {:key "test" :value "a"} :next nil}
                     :tail {:value {:key "test" :value "a"} :next nil}}
                    {:head nil :tail nil}]}
         (-> table
             (ht/set* "test" "a")
             (ht/set* "abc" 10)
             (ht/set* "efg" 'v)
             record->map)))
  (is (= {:keys {"test" 1 "abc" 0 "efg" 0}
          :buckets [{:head {:value {:key "efg" :value 'v}
                            :next {:value {:key "abc" :value 210} :next nil}}
                     :tail {:value {:key "abc" :value 210} :next nil}}
                    {:head {:value {:key "test" :value "a"} :next nil}
                     :tail {:value {:key "test" :value "a"} :next nil}}
                    {:head nil :tail nil}]}
         (-> table
             (ht/set* "test" "a")
             (ht/set* "abc" 10)
             (ht/set* "efg" 'v)
             (ht/set* "abc" 210)
             record->map))))

(deftest delete-test
  (is (= [table nil]
         (ht/delete table "abc")))
  (is (= [{:keys {"abc" 0}
           :buckets [{:head {:value {:key "abc" :value 10} :next nil}
                      :tail {:value {:key "abc" :value 10} :next nil}}
                     {:head nil :tail nil}
                     {:head nil :tail nil}]}
          nil]
         (-> table
             (ht/set* "abc" 10)
             (ht/delete "cde")
             record->map)))
  (is (= [{:keys {}
           :buckets [{:head nil :tail nil}
                     {:head nil :tail nil}
                     {:head nil :tail nil}]}
          {:value {:key "abc" :value 10} :next nil}]
         (-> table
             (ht/set* "abc" 10)
             (ht/delete "abc")
             record->map)))
  (is (= [{:keys {"test" 1 "abc" 0}
           :buckets [{:head {:value {:key "abc" :value 10} :next nil}
                      :tail {:value {:key "abc" :value 10} :next nil}}
                     {:head {:value {:key "test" :value "a"} :next nil}
                      :tail {:value {:key "test" :value "a"} :next nil}}
                     {:head nil :tail nil}]}
          {:value {:key "efg" :value 'v} :next nil}]
         (-> table
             (ht/set* "test" "a")
             (ht/set* "abc" 10)
             (ht/set* "efg" 'v)
             (ht/delete "efg")
             record->map))))

(deftest get*-test
  (is (= nil
         (ht/get* table "abc")))
  (is (= nil
         (-> table
             (ht/set* "abc" 10)
             (ht/get* "efg"))))
  (is (= 10
         (-> table
             (ht/set* "abc" 10)
             (ht/get* "abc"))))
  (is (= 'v
         (-> table
             (ht/set* "test" "a")
             (ht/set* "abc" 10)
             (ht/set* "efg" 'v)
             (ht/get* "efg")))))

(deftest has-test
  (is (= false
         (ht/has table "abc")))
  (is (= false
         (-> table
             (ht/set* "abc" 0)
             (ht/has "efg"))))
  (is (= true
         (-> table
             (ht/set* "abc" 0)
             (ht/has "abc"))))
  (is (= true
         (-> table
             (ht/set* "abc" 0)
             (ht/set* "test" 1)
             (ht/has "test")))))

(deftest get-keys-test
  (is (= nil
         (ht/get-keys table)))
  (is (= '("abc" "efg" "test")
         (-> table
             (ht/set* "abc" 0)
             (ht/set* "efg" 1)
             (ht/set* "test" 2)
             (ht/get-keys)))))
