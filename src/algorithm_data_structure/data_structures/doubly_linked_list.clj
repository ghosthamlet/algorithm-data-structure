(ns algorithm-data-structure.data-structures.doubly-linked-list
  (:require [algorithm-data-structure.data-structures.doubly-linked-list-node :as dlln]))

(defn create [comparator-function]
  {:head (atom nil)
   :tail (atom nil)
   :compare comparator-function})

(defn prepend [dll value]
  (let [new-node (dlln/create value (:head dll))]
    (when @(:head dll)
      (reset! (:previous @(:head dll)) new-node))
    (reset! (:head dll) new-node)
    (when-not @(:tail dll)
      (reset! (:tail dll) new-node))
    dll))

(defn append [dll value]
  (let [new-node (dlln/create value)]
    (if-not @(:head dll)
      (do
        (reset! (:head dll) new-node)
        (reset! (:tail dll) new-node))
      (do (reset! (:next @(:tail dll)) new-node)
          (reset! (:previous new-node) @(:tail dll))
          (reset! (:tail dll) new-node)))
    dll))

(defn delete [dll value]
  (if-not @(:head dll)
    [dll nil]
    (let [deleted-node (atom nil)
          current-node (:head dll)]
      (while @current-node
        (when (compare-equal (:value @current-node) value)
          (reset! deleted-node current-node)
          (cond
            (= @deleted-node @(:head dll))
            (do
              (reset! (:head dll) @(:next @deleted-node))
              (when @(:head dll)
                (reset! (:previous @(:head dll)) nil))
              (when (= @deleted-node @(:tail dll))
                (reset! (:tail dll) nil)))
            (= @deleted-node @(:tail dll))
            (do
              (reset! (:tail dll) @(:previous @deleted-node))
              (reset! (:next @(:tail dll)) nil))
            :else
            (let [previous-node (:previous @deleted-node)
                  next-node (:next @deleted-node)]
              (reset! (:next @previous-node) @next-node)
              (reset! (:previous @next-node) @previous-node))))
        (reset! current-node (:next @current-node)))
      [dll @deleted-node])))

(defn find* [dll & {:keys [value callback]
                    :or {value nil
                         callback nil}}]
  (when @(:head dll)
    (loop [node @(:head dll)]
      (when node
        (if (and callback (callback (:value node)))
          node
          (if (and value (compare-equal (:value node) value))
            node
            (recur @(:next node))))))))

(defn delete-tail [dll]
  [dll
   (when @(:tail dll)
     (let [deleted-tail (:tail dll)]
       (if (= @(:head dll) @(:tail dll))
         (do
           (reset! (:head dll) nil)
           (reset! (:tail dll) nil))
         (do
           (reset! deleted-tail @(:previous @deleted-tail))
           (reset! (:next @deleted-tail) nil)))
       @deleted-tail))])

(defn delete-head [dll]
  [dll
   (when @(:head dll)
     (let [deleted-head (:head dll)]
       (if @(:next @(:head dll))
         (do
           (reset! deleted-head @(:next @deleted-head))
           (reset! (:previous @deleted-head) nil))
         (do
           (reset! deleted-head nil)
           (reset! (:tail dll) nil)))
       @deleted-head))])

(defn ->array [dll]
  (loop [nodes []
         current-node @(:head dll)]
    (if-not current-node
      nodes
      (recur (conj nodes current-node)
             @(:next current-node)))))

(defn ->string [dll callback]
  (str (map #(dlln/->string % callback) (->array dll))))
