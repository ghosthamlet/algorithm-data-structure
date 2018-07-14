(ns algorithm-data-structure.data-structures.trie-node
  (:require [algorithm-data-structure.data-structures.hash-table :as ht]))

(defn create
  ([character] (create character false))
  ([character is-complete-word]
   {:character character
    :is-complete-word is-complete-word
    :children (ht/create)}))

(defn get-child [tn character]
  (ht/get (:children tn) character))

(defn add-child
  ([tn character] (add-child tn character false))
  ([tn character is-complete-word]
   [(if (has-child tn character)
      tn
      (update tn :children
              ht/set character (create character is-complete-word)))
    (get-child tn character)]))

(defn has-child [tn character]
  (ht/has (:children tn) character))

(defn suggest-children [tn]
  (ht/get-keys (:children tn)))

(defn ->string [tn]
  (let [children-as-string (str (suggest-children tn))
        children-as-string (if (seq? children-as-string)
                             (str ":" children-as-string)
                             "")
        is-complete-string (if (:is-complete-word tn) "*" "")]
    (str (:character tn) is-complete-string children-as-string)))
