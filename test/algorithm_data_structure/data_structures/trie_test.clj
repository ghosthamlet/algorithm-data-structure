(ns algorithm-data-structure.data-structures.trie-test
  (:require [algorithm-data-structure.data-structures.trie :as t]
            [algorithm-data-structure.data-structures.trie-node :as tn]
            [clojure.test :refer :all]))

(def trie (t/create))

(deftest create-test
  (is (= {:head (tn/create t/head-character)}
         trie)))

(deftest add-word-test
  (is (= {:head (-> (tn/create t/head-character)
                    (tn/add-child "a" true)
                    first)}
         (-> trie
             (t/add-word "a")))))

(deftest get-last-character-node-test
  (is (= nil
         (-> trie
             (t/get-last-character-node "abc"))))
  (is (= (tn/create "c" true)
         (-> trie
             (t/add-word "abc")
             (t/get-last-character-node "abc"))))
  (is (= (tn/create "c" true)
         (-> trie
             (t/add-word "abc")
             (t/add-word "egd")
             (t/get-last-character-node "abc")))))

(deftest does-word-exist-test
  (is (= false
         (-> trie
             (t/does-word-exist "abc"))))
  (is (= true
         (-> trie
             (t/add-word "abc")
             (t/does-word-exist "abc"))))
  (is (= true
         (-> trie
             (t/add-word "abc")
             (t/add-word "egd")
             (t/does-word-exist "egd")))))

(deftest suggest-next-characters-test
  (is (= nil
         (-> trie
             (t/suggest-next-characters "egd"))))
  (is (= nil
         (-> trie
             (t/add-word "abc")
             (t/add-word "egd")
             (t/suggest-next-characters "egd"))))
  (is (= ["b" "f"]
         (-> trie
             (t/add-word "abc")
             (t/add-word "afg")
             (t/suggest-next-characters "a")))))

