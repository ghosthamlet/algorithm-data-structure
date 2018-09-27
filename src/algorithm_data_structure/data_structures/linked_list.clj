(ns algorithm-data-structure.data-structures.linked-list
  (:require [algorithm-data-structure.data-structures.linked-list-node :as lln]
            [algorithm-data-structure.comparator :refer :all]))

(defn create []
  {:head nil
   :tail nil})

(defn get-link-path [self]
  (loop [next (:head self)
         path [:head]]
    (if-not next
      path
      (recur (:next next)
             (conj path :next)))))

(defn prepend [self value]
  (let [new-node (lln/create value (:head self))
        self (assoc self
                    :head new-node)]
    (if-not (:tail self)
      (assoc self
             :tail new-node)
      self)))

(defn append [self value]
  (let [new-node (lln/create value)]
    (if-not (:head self)
      (assoc self
             :head new-node
             :tail new-node)
      (-> self
          (assoc-in
           (get-link-path self) new-node)
          (assoc
           :tail new-node)))))

(defn delete [self value]
  (if (:head self)
    (let [[self deleted-node]
          (loop [self self
                 deleted-node nil]
            (if (and (:head self)
                     (equal self
                            (get-in self [:head :value])
                            value))
              (recur (update self :head :next)
                     (:head self))
              [self deleted-node]))
          current-node (:head self)
          [self current-node deleted-node]
          (if current-node
            (loop [self self
                   current-node current-node
                   deleted-node deleted-node
                   path [:head]]
              (if (nil? (:next current-node))
                [self current-node deleted-node]
                (let [[self current-node deleted-node path]
                      (if (equal self
                                 (get-in current-node
                                         [:next :value])
                                 value)
                        [(update-in self
                                    path update :next :next)
                         (update current-node :next :next)
                         (:next current-node)
                         path]
                        [self
                         (:next current-node)
                         deleted-node
                         (conj path :next)])]
                  (recur self
                         current-node
                         deleted-node
                         path))))
            [self current-node deleted-node])
          self (if (equal self
                          (get-in self [:tail :value])
                          value)
                 (assoc self :tail current-node)
                 self)]
      [self deleted-node])
    [self nil]))

(defn find* [self value callback]
  (when (:head self)
    (loop [find? false
           ret (:head self)
           current-node (:head self)]
      (if find?
        ret
        (when current-node
          (recur (-> value
                     (and (equal self
                                 (:value current-node) value))
                     (or (and callback
                              (callback (:value current-node)))))
                 current-node
                 (:next current-node)))))))

(defn delete-tail [self]
  (if (= (:head self) (:tail self))
    [(assoc self
            :head nil
            :tail nil)
     (:tail self)]
    (loop [self self
           current-node (:head self)
           path [:head]]
      (if (:next current-node)
        (let [[self current-node path]
              (if-not (get-in current-node [:next :next])
                [(assoc-in self (conj path :next) nil)
                 (assoc current-node :next nil)
                 path]
                [self
                 (:next current-node)
                 (conj path :next)])]
          (recur self
                 current-node
                 path))
        [(assoc self :tail current-node)
         (:tail self)]))))

(defn delete-head [self]
  (if (:head self)
    (let [head (get-in self [:head :next])]
      [(assoc self
              :head head
              :tail (when head (:tail self)))
       (:head self)])
    [self nil]))

(defn ->array [self]
  (loop [nodes []
         current-node (:head self)]
    (if (nil? current-node)
      nodes
      (recur (conj nodes current-node)
             (:next current-node)))))

(defn ->string [self & [callback]]
  (map #(lln/->string % callback) (->array self)))
