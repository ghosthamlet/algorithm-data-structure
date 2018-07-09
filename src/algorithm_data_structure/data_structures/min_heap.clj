(ns algorithm-data-structure.data-structures.min-heap
  (:require [algorithm-data-structure.util :refer :all]))

;; TODO: dynamic bind default compare to remove pass compare everywhere
(def ^:dynamic *compare*)

;; TODO: use defrecored MinHeap, implement nth count...
(defn create []
  {:heap-container []})

(defn- cont [heap]
  (:heap-container heap))

(defn- len* [heap]
  (count (cont heap)))

(defn- nth* [heap n]
  (nth (cont heap) n))

(defn assoc-cont
  [heap & kvs]
  (->> kvs
       (apply assoc (cont heap))
       (if (< (count kvs) 2) (first kvs))
       (assoc heap :heap-container)))

(defn update-cont [heap f & args]
  (apply update heap :heap-container f args))

(defn get-left-child-index [parent-index]
  (inc (* 2 parent-index)))

(defn get-right-child-index [parent-index]
  (inc (get-left-child-index parent-index)))

(defn get-parent-index [child-index]
  (int (Math/floor (/ (dec child-index) 2))))

(defn has-parent [child-index]
  (>= (get-parent-index child-index) 0))

(defn has-left-child [heap parent-index]
  (< (get-left-child-index parent-index) (len* heap)))

(defn has-right-child [heap parent-index]
  (< (get-right-child-index parent-index) (len* heap)))

(defn left-child [heap parent-index]
  (nth* heap (get-left-child-index parent-index)))

(defn right-child [heap parent-index]
  (nth* heap (get-right-child-index parent-index)))

(defn parent [heap child-index]
  (nth* heap (get-parent-index child-index)))

(defn swap [heap index-one index-two]
  (assoc-cont heap
              index-one (nth* heap index-two)
              index-two (nth* heap index-one)))

(defn heapify-up
  ([heap compare]
   (heapify-up heap compare (dec (len* heap))))
  ([heap compare custom-start-index]
   (if (and (has-parent custom-start-index)
            (compare :less-then
                     (nth* heap custom-start-index)
                     (parent heap custom-start-index)))
     (recur (swap heap
                  custom-start-index
                  (get-parent-index custom-start-index))
            compare
            (get-parent-index custom-start-index))
     heap)))

(defn heapify-down
  ([heap compare]
   (heapify-down heap compare 0))
  ([heap compare custom-start-index]
   (if (has-left-child heap custom-start-index)
     (let [next-index (if (and (has-right-child heap custom-start-index)
                               (compare :less-then
                                        (right-child heap custom-start-index)
                                        (left-child heap custom-start-index)))
                        (get-right-child-index custom-start-index)
                        (get-left-child-index custom-start-index))]
       (if (compare :less-then
                    (nth* heap custom-start-index)
                    (nth* heap next-index))
         heap
         (recur (swap heap custom-start-index next-index)
                compare
                next-index)))
     heap)))

(defn peek [heap]
  (when (pos? (len* heap))
    (first (cont heap))))

(defn poll [heap compare]
  (case (len* heap)
    0 [heap nil]
    1 (update ((juxt butlast last) (cont heap))
              0 #(assoc-cont heap (vec %)))
    [(heapify-down (assoc-cont heap (move (cont heap) :last 0)) compare)
     (first (cont heap))]))

(defn add [heap item compare]
  (heapify-up (update-cont heap conj item) compare))

(defn find*
  [heap item compare custom-compare]
  (let [custom-compare (or custom-compare compare)
        len (len* heap)]
    (loop [item-index 0
           found-item-indices []]
      (if (= item-index len)
        found-item-indices
        (recur (inc item-index)
               (if (custom-compare :equal item (nth* heap item-index))
                 (conj found-item-indices item-index)
                 found-item-indices))))))

(defn remove*
  [heap item compare custom-compare]
  (let [custom-compare (or custom-compare compare)
        number-of-items-to-remove (count (find* heap item compare custom-compare))]
    (loop [iteration 0
           heap heap]
      (if (= iteration number-of-items-to-remove)
        heap
        (recur (inc iteration)
               (let [index-to-remove (last (find* heap item compare custom-compare))]
                 (if (->> (cont heap) count dec (= index-to-remove))
                   (update-cont heap butlast)
                   (let [heap-container (move (cont heap) :last index-to-remove)
                         parent-item (when (has-parent index-to-remove)
                                       (parent heap index-to-remove))
                         left-child-item (when (has-left-child heap
                                                               index-to-remove)
                                           (left-child heap index-to-remove))]
                     (if (->> index-to-remove
                              (nth heap-container)
                              (compare :less-then parent-item)
                              (or parent-item)
                              (and left-child-item))
                       (heapify-down (assoc-cont heap heap-container) compare index-to-remove)
                       (heapify-up (assoc-cont heap heap-container) compare index-to-remove))))))))))

(defn empty?* [heap]
  (zero? (len* heap)))

(defn ->string [heap]
  (str (cont heap)))
