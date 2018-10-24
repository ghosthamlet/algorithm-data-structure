(ns algorithm-data-structure.data-structures.trie-node
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/trie"
  (:require [algorithm-data-structure.data-structures.hash-table :as ht]))

(declare has-child)

(defn create
  ([character] (create character false))
  ([character is-complete-word]
   {:character character
    :is-complete-word is-complete-word
    :children (ht/create)}))

(defn get-child [self character]
  (ht/get* (:children self) character))

(defn add-child
  ([self character] (add-child self character false))
  ([self character is-complete-word]
   (let [self (if (has-child self character)
                self
                (update self :children
                        ht/set* character (create character is-complete-word)))]
     [self (get-child self character)])))

(defn add-child-node [self node]
  (update self :children ht/set* (:character node) node))

(defn has-child [self character]
  (ht/has (:children self) character))

(defn suggest-children [self]
  (ht/get-keys (:children self)))

(defn ->string [self]
  (let [children-as-string (str (suggest-children self))
        children-as-string (if (empty? children-as-string) "" (str ":" children-as-string))
        is-complete-string (if (:is-complete-word self) "*" "")]
    (str (:character self) is-complete-string children-as-string)))
