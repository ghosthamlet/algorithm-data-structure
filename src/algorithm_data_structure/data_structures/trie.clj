(ns algorithm-data-structure.data-structures.trie
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/trie"
  (:require [algorithm-data-structure.data-structures.trie-node :as tn]))

(def head-character "*")

(defn create []
  {:head (tn/create head-character)})

(defn add-word [self word]
  (let [characters (vec (map str (seq word)))
        len (count characters)
        leni (dec len)
        f (fn f[node i]
            (let [[parent node] (tn/add-child node (characters i) (= i leni))]
              (if (= i leni)
                parent
                (tn/add-child-node parent (f node (inc i))))))]
    (update self :head f 0)))

(declare get-last-character-node)

(defn suggest-next-characters [self word]
  (let [last-character (get-last-character-node self word)]
    (when last-character
      (tn/suggest-children last-character))))

(defn does-word-exist [self word]
  (boolean (get-last-character-node self word)))

(defn get-last-character-node [self word]
  (let [characters (vec (map str (seq word)))
        len (count characters)]
    (loop [char-index 0
           current-node (:head self)]
      (if (= char-index len)
        current-node
        (when (tn/has-child current-node (characters char-index))
          (recur (inc char-index)
                 (tn/get-child current-node (characters char-index))))))))
