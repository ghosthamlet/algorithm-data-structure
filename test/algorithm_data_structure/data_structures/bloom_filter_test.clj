(ns algorithm-data-structure.data-structures.bloom-filter-test
  (:require [algorithm-data-structure.data-structures.bloom-filter :as bf]
            [clojure.test :refer :all]))

(def bloom (bf/create 100))

(deftest create-test
  (is (= {:size 100
          :storage (vec (repeat 100 false))}
         (bf/create))))

(deftest set-value-test
  (is (= {:size 100
          :storage (assoc (vec (repeat 100 false)) 0 true)}
         (-> bloom
             (#'bf/set-value 0)))
      "test private fn"))

(deftest get-value-test
  (is (= false
         (-> bloom
             (#'bf/get-value 0)))
      "test private fn"))


(deftest hash1-test
  (is (= 66
         (-> bloom
             (bf/hash1 "abc")))))

(deftest hash2-test
  (is (= 63
         (-> bloom
             (bf/hash2 "abc")))))

(deftest hash3-test
  (is (= 54
         (-> bloom
             (bf/hash3 "abc")))))

(deftest get-hash-values-test
  (is (= [66 63 54]
         (-> bloom
             (bf/get-hash-values "abc")))))

(deftest insert-test
  (is (= {:size 100
          :storage (assoc (vec (repeat 100 false))
                          66 true
                          63 true
                          54 true)}
       (-> bloom
           (bf/insert "abc")))))

(deftest may-contain-test
  (is (= false
         (-> bloom
             (bf/may-contain ""))))
  (is (= false
         (-> bloom
             (bf/may-contain "abc"))))
  (is (= false
         (-> bloom
             (bf/insert "ab")
             (bf/may-contain "abc"))))
  (is (= true
         (-> bloom
             (bf/insert "abc")
             (bf/may-contain "abc"))))
  (is (= true
         (-> bloom
             (bf/insert "abc")
             (bf/insert "def")
             (bf/may-contain "abc"))))
  (is (= true
         (-> bloom
             (bf/insert "abc")
             (bf/insert "def")
             (bf/insert "ghi")
             (bf/may-contain "def")))))
