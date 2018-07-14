(ns algorithm-data-structure.data-structures.trie
  (:require [algorithm-data-structure.data-structures.trie-node :as tn]))

(defn head-character "*")

(defn create []
  (:head (tn/create head-character)))

(defn add-word [trie word]
  (let [characters (vec (seq word))
        len (count characters)
        leni (dec len)]
    (reduce #(update %1 :head
                     tn/add-child (characters %2) (= %2 leni))
            trie
            (range len))))

(declare get-last-character-node)

(defn suggest-next-characters [trie word]
  (let [last-character (get-last-character-node trie word)]
    (when last-character
      (tn/suggest-children last-character))))

(defn does-word-exist [trie word]
  (boolean (get-last-character-node trie word)))

(defn get-last-character-node [trie word]
  (let [characters (seq word)
        len (count characters)]
    (loop [char-index 0
           current-node (:head trie)]
      (if (= char-index len)
        current-node
        (when (tn/has-child current-node (characters char-index))
          (recur (inc char-index)
                 (tn/get-child current-node (characters char-index))))))))
