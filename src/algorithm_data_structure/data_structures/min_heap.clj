(ns algorithm-data-structure.data-structures.min-heap
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/heap"
  (:require [algorithm-data-structure.comparator :refer :all]
            [algorithm-data-structure.util :refer :all]))

;; XXX: dynamic bind default compare to remove pass compare everywhere?
(def ^:dynamic *compare*)

;; TODO: use defrecored MinHeap, implement nth count...
(defn create []
  {:heap-container []})

(defn- cont [self]
  (:heap-container self))

(defn- len* [self]
  (count (cont self)))

(defn- nth* [self n]
  (nth (cont self) n))

(defn assoc-cont
  [self & kvs]
  (->> kvs
       ;; (do (prn (cont self)))
       (apply assoc (vec (cont self)))
       (if (< (count kvs) 2) (first kvs))
       (assoc self :heap-container)))

(defn update-cont [self f & args]
  (apply update self :heap-container f args))

(defn get-left-child-index [parent-index]
  (inc (* 2 parent-index)))

(defn get-right-child-index [parent-index]
  (inc (get-left-child-index parent-index)))

(defn get-parent-index [child-index]
  (int (Math/floor (/ (dec child-index) 2))))

(defn has-parent [child-index]
  (>= (get-parent-index child-index) 0))

(defn has-left-child [self parent-index]
  (< (get-left-child-index parent-index) (len* self)))

(defn has-right-child [self parent-index]
  (< (get-right-child-index parent-index) (len* self)))

(defn left-child [self parent-index]
  (nth* self (get-left-child-index parent-index)))

(defn right-child [self parent-index]
  (nth* self (get-right-child-index parent-index)))

(defn parent [self child-index]
  (nth* self (get-parent-index child-index)))

(defn swap [self index-one index-two]
  (assoc-cont self
              index-one (nth* self index-two)
              index-two (nth* self index-one)))

(defn selfify-up
  ([self]
   (selfify-up self (dec (len* self))))
  ([self custom-start-index]
   (if (and (has-parent custom-start-index)
            (less-then self
                     (nth* self custom-start-index)
                     (parent self custom-start-index)))
     (recur (swap self
                  custom-start-index
                  (get-parent-index custom-start-index))
            (get-parent-index custom-start-index))
     self)))

(defn selfify-down
  ([self]
   (selfify-down self 0))
  ([self custom-start-index]
   (if (has-left-child self custom-start-index)
     (let [next-index (if (and (has-right-child self custom-start-index)
                               (less-then self
                                          (right-child self custom-start-index)
                                          (left-child self custom-start-index)))
                        (get-right-child-index custom-start-index)
                        (get-left-child-index custom-start-index))]
       (if (less-then self
                    (nth* self custom-start-index)
                    (nth* self next-index))
         self
         (recur (swap self custom-start-index next-index)
                next-index)))
     self)))

(defn peek [self]
  (when (pos? (len* self))
    (first (cont self))))

(defn poll [self]
  (case (len* self)
    0 [self nil]
    1 (update ((juxt butlast last) (cont self))
              0 #(assoc-cont self (vec %)))
    [(selfify-down (assoc-cont self (move (cont self) :last 0)))
     (first (cont self))]))

(defn add [self item]
  (selfify-up (update-cont self conj item)))

(defn find*
  [self item custom-compare]
  (let [len (len* self)]
    (loop [item-index 0
           found-item-indices []]
      (if (= item-index len)
        found-item-indices
        (recur (inc item-index)
               (if (equal self item (nth* self item-index) custom-compare)
                 (conj found-item-indices item-index)
                 found-item-indices))))))

(defn remove*
  [self item custom-compare]
  (let [number-of-items-to-remove (count (find* self item custom-compare))]
    (loop [iteration 0
           self self]
      (if (= iteration number-of-items-to-remove)
        self
        (recur (inc iteration)
               (let [index-to-remove (last (find* self item custom-compare))]
                 (if (->> (cont self) count dec (= index-to-remove))
                   (update-cont self butlast)
                   (let [self-container (move (cont self) :last index-to-remove)
                         parent-item (when (has-parent index-to-remove)
                                       (parent self index-to-remove))
                         left-child-item (when (has-left-child self index-to-remove)
                                           (left-child self index-to-remove))]
                     (if (and left-child-item
                              (or (not parent-item)
                                  (->> index-to-remove
                                       (nth self-container)
                                       (less-then self parent-item))))
                       (selfify-down (assoc-cont self self-container) index-to-remove)
                       (selfify-up (assoc-cont self self-container) index-to-remove))))))))))

(defn empty?* [self]
  (zero? (len* self)))

(defn ->string [self]
  (str (cont self)))
