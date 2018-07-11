(ns algorithm-data-structure.data-structures.linked-list
  (:require [algorithm-data-structure.data-structures.linked-list-node :as llnode]))

(defn create []
  {:head nil
   :tail nil})

(defn prepend [llist value]
  (update llist
          :head #(llnode/create value %)))

(defn append [llist value]
  (let [new-node (llnode/create value)]
    (if-not (:head llist)
      (assoc llist
             :head new-node
             :tail new-node)
      (-> llist
          (update-in
           [:tail :next] new-node)
          (assoc
           :tail new-node)))))

(defn delete [llist value compare]
  (if (:head llist)
    (let [[llist deleted-node] (if (compare :equal
                                            (get-in llist [:head :value])
                                            value)
                                 [(update llist :head :next)
                                  (:head llist)]
                                 [llist nil])
          current-node (:head llist)
          [current-node deleted-node] (if current-node
                                        (loop [cnext (:next current-node)
                                               current-node current-node
                                               deleted-node deleted-node]
                                          (if (nil? cnext)
                                            [current-node deleted-node]
                                            (let [[current-node deleted-node]
                                                  (if (compare :equal
                                                               (get-in current-node
                                                                       [:next :value])
                                                               value)
                                                    [(update current-node
                                                             :next :next)
                                                     (:next current-node)]
                                                    [(:next current-node)
                                                     deleted-node])]
                                              (recur (:next current-node)
                                                     current-node
                                                     deleted-node))))
                                        [current-node deleted-node])
          llist (if (compare :equal
                             (get-in llist [:tail :value])
                             value)
                  (assoc llist :tail current-node)
                  llist)]
      [llist deleted-node])
    [llist nil]))

(defn find* [llist value callback compare]
  (when (:head llist)
    (loop [find? false
           current-node (:head llist)
           nnode current-node]
      (if find?
        current-node
        (when nnode
          (recur (->> value
                      (compare :equal
                               (:value current-node))
                      (and value)
                      (or (and callback
                               (callback (:value current-node)))))
                 nnode
                 (:next nnode)))))))

(defn delete-tail [llist]
  (if (= (:head llist) (:tail llist))
    [(assoc llist
            :head nil
            :tail nil)
     (:tail llist)]
    (loop [current-node (:head llist)]
      (if (:next current-node)
        (recur (if (get-in current-node [:next :next])
                 (:next current-node)
                 (assoc current-node :next nil)))
        [(assoc llist :tail current-node)
         (:tail llist)]))))

(defn delete-head [llist]
  (if (:head llist)
    (let [head (get-in llist [:head :next])]
      [(assoc llist
              :head head
              :tail (when head (:tail llist)))
       (:head llist)])
    [llist nil]))

(defn ->array [llist]
  (loop [nodes []
         current-node (:head llist)]
    (if (nil? current-node)
      nodes
      (recur (conj nodes current-node)
             (:next current-node)))))

(defn ->string [llist callback]
  (map #(llnode/->string % callback) (->array llist)))
