(ns algorithm-data-structure.data-structures.doubly-linked-list
  "Adapted from https://rosettacode.org/wiki/Doubly-linked_list/Definition#Clojure not the same as js version
   https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/doubly-linked-list"
  (:require [algorithm-data-structure.data-structures.doubly-linked-list-node :as dlln]
            [algorithm-data-structure.comparator :refer :all]))

(defprotocol PDoublyLinkedList
  (prepend [this value])
  (append [this value])
  (delete [this value])
  (find* [this value callback])
  (delete-tail [this])
  (delete-head [this])
  (->array [this])
  (->string [this callback])

  (delete-node [this node])
  (get-head [this])
  (get-tail [this])
  (add-before [this node value])
  (add-after [this node value])
  (get-nth [this n]))

(defn- seq* [m start next]
  (seq
   (for [node (iterate #(get m (next %)) (get m start))
         :while node]
     (:value node))))

(defmacro when->
  ([x pred form] `(let [x# ~x] (if ~pred (-> x# ~form) x#)))
  ([x pred form & more] `(when-> (when-> ~x ~pred ~form) ~@more)))

(deftype DoublyLinkedList [m head tail]
  Object
    (equals [this dll]
      (and (instance? DoublyLinkedList dll)
           (= m (.m ^DoublyLinkedList dll))))
    ;; (hashCode [this] (hash (or this ())))
  clojure.lang.Sequential
  clojure.lang.Counted
    (count [_] (count m))
  clojure.lang.Seqable
    (seq [_] (seq* m head :next))
  clojure.lang.Reversible
  (rseq [_] (seq* m tail :previous))
  clojure.lang.IPersistentCollection
    (empty [_] (DoublyLinkedList. (empty m) nil nil))
    (equiv [this dll]
      (and (sequential? dll)
           (= (seq dll) (seq this))))
    (cons [this value] (.append this value))

  PDoublyLinkedList
    (prepend [this value]
      (let [new-key (Object.)
            m (when-> (assoc m new-key (dlln/create nil head value))
                head (assoc-in [head :previous] new-key))
            tail (if tail tail new-key)]
        (DoublyLinkedList. m new-key tail)))
    (append [this value]
      (if-let [tail (.get-tail this)]
        (.add-after this tail value)
        (.prepend this value)))
    (delete [this value]
      (reduce (fn [acc {:keys [previous next key]
                        :as node}]
                (.delete-node acc node)
                #_(let [m (.m acc)
                        head (.head acc)
                        tail (.tail acc)]
                    (if (m key)
                      (let [head (if previous head next)
                            tail (if next tail previous)
                            m (when-> (dissoc m key)
                                previous (assoc-in [previous :next] next)
                                next (assoc-in [next :previous] previous))]
                        (DoublyLinkedList. m head tail))
                      acc)))
              this (dlln/get-nodes-by-value m value)))
    (find* [this value callback]
      (let [callback (or callback equal)]
        (if-let [node (when (and tail (callback this value (:value tail)))
                        tail)]
          node
          (->> m
               (filter #(callback this (:value (second %)) value))
               first
               second))))
    (delete-tail [this]
      (.delete-node this (.get-tail this)))
    (delete-head [this]
      (.delete-node this (.get-head this)))
    (->array [this]
      (seq this))
    (->string [this callback]
      (str (vec (->array this))))

    (delete-node [this {:keys [previous next key]}]
      (if (get m key)
        (let [head (if previous head next)
              tail (if next tail previous)
              m (when-> (dissoc m key)
                  previous (assoc-in [previous :next] next)
                  next (assoc-in [next :previous] previous))]
          (DoublyLinkedList. m head tail))
        this))
    (get-head [_] (dlln/get-node m head))
    (get-tail [_] (dlln/get-node m tail))
    (add-after [this node value]
      (if (get m (:key node))
        (let [{:keys [next key]} node
              new-key (Object.)
              m (when-> (-> (assoc m new-key (dlln/create key next value))
                            (assoc-in , [key :next] new-key))
                  next (assoc-in [next :previous] new-key))
              tail (if next tail new-key)]
          (DoublyLinkedList. m head tail))
        this))
    (add-before [this node value]
      (if (:previous node)
        (.add-after this (dlln/get-prev node) value)
        (.prepend this value)))
    (get-nth [this n]
      (let [nth-key (fn [self n]
                      (if (< -1 n (.count self))
                        (let [[start next n] (if (< n (/ (.count self) 2))
                                               [(.head self) :next n]
                                               [(.tail self) :previous (- (.count self) n 1)])]
                          (nth (iterate #(get-in (.m self) [% next]) start) n))
                        (throw (IndexOutOfBoundsException.))))]
        (dlln/get-node m (nth-key this n)))))

(defn create
  ([] (DoublyLinkedList. nil nil nil))
  ([coll] (into (create) coll)))



(comment
  "Failed to imitate js version"

  (defn create []
    {:head (atom nil)
     :tail (atom nil)})

  (defn prepend [self value]
    (let [new-node (dlln/create value (:head self))]
      (when @(:head self)
        (reset! (:previous @(:head self)) new-node))
      (reset! (:head self) new-node)
      (when-not @(:tail self)
        (reset! (:tail self) new-node))
      self))

  (defn append [self value]
    (let [new-node (dlln/create value)]
      (if-not @(:head self)
        (do
          (reset! (:head self) new-node)
          (reset! (:tail self) new-node))
        (do
          (reset! (:next @(:tail self)) new-node)
          (reset! (:previous new-node) @(:tail self))
          (reset! (:tail self) new-node)
          (reset! (:next @(:head self)) new-node)))
      self))

  (defn delete [self value]
    (if-not @(:head self)
      [self nil]
      (let [deleted-node (atom nil)
            current-node (:head self)]
        (while @current-node
          (when (equal self (:value @current-node) value)
            (reset! deleted-node current-node)
            (cond
              (= @deleted-node @(:head self))
              (do
                (reset! (:head self) @(:next @deleted-node))
                (when @(:head self)
                  (reset! (:previous @(:head self)) nil))
                (when (= @deleted-node @(:tail self))
                  (reset! (:tail self) nil)))
              (= @deleted-node @(:tail self))
              (do
                (reset! (:tail self) @(:previous @deleted-node))
                (reset! (:next @(:tail self)) nil))
              :else
              (let [previous-node (:previous @deleted-node)
                    next-node (:next @deleted-node)]
                (reset! (:next @previous-node) @next-node)
                (reset! (:previous @next-node) @previous-node))))
          (reset! current-node (:next @current-node)))
        [self @deleted-node])))

  (defn find* [self & {:keys [value callback]
                       :or {value nil
                            callback nil}}]
    (when @(:head self)
      (loop [node @(:head self)]
        (when node
          (if (and callback (callback (:value node)))
            node
            (if (and value (equal self (:value node) value))
              node
              (recur @(:next node))))))))

  (defn delete-tail [self]
    [self
     (when @(:tail self)
       (let [deleted-tail (:tail self)]
         (if (= @(:head self) @(:tail self))
           (do
             (reset! (:head self) nil)
             (reset! (:tail self) nil))
           (do
             (reset! deleted-tail @(:previous @deleted-tail))
             (reset! (:next @deleted-tail) nil)))
         @deleted-tail))])

  (defn delete-head [self]
    [self
     (when @(:head self)
       (let [deleted-head (:head self)]
         (if @(:next @(:head self))
           (do
             (reset! deleted-head @(:next @deleted-head))
             (reset! (:previous @deleted-head) nil))
           (do
             (reset! deleted-head nil)
             (reset! (:tail self) nil)))
         @deleted-head))])

  (defn ->array [self]
    (loop [nodes []
           current-node @(:head self)]
      (if-not current-node
        nodes
        (recur (conj nodes current-node)
               @(:next current-node)))))

  (defn ->string [self callback]
    (str (map #(dlln/->string % callback) (->array self))))
  )
