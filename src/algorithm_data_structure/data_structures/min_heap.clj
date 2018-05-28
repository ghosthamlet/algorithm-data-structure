(ns algorithm-data-structure.data-structures.min-heap
  (:require [algorithm-data-structure.util :refer :all]))

;; TODO: dynamic bind default compare to remove pass compare everywhere
(def ^:dynamic *compare*)

;; TODO: use defrecored MinHeap, implement nth count...
(defn create []
  {:heap-container []})

(defn cont [heap]
  (:heap-container heap))

(defn lenh [heap]
  (count (cont heap)))

(defn nthh [heap n]
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
  (+ (* 2 parent-index) 2))

(defn get-parent-index [child-index]
  (Math/floor (/ (dec child-index) 2)))

(defn has-parent [child-index]
  (>= (get-parent-index child-index) 0))

(defn has-left-child [heap parent-index]
  (< (get-left-child-index parent-index) (lenh heap)))

(defn has-right-child [heap parent-index]
  (< (get-right-child-index parent-index) (lenh heap)))

(defn left-child [heap parent-index]
  (nthh heap (get-left-child-index parent-index)))

(defn right-child [heap parent-index]
  (nthh heap (get-right-child-index parent-index)))

(defn parent [heap child-index]
  (nthh heap (get-parent-index child-index)))

(defn swap [heap index-one index-two]
  (assoc-cont heap
              index-one (nthh heap index-two)
              index-two (nthh heap index-one)))

(defn heapify-up
  ([heap compare]
   (heapify-up heap compare (dec (lenh heap))))
  ([heap compare custom-start-index]
   (if (and (has-parent custom-start-index)
            (compare :less-then
                     (nthh heap custom-start-index)
                     (parent custom-start-index)))
     (recur (swap heap
                  custom-start-index
                  (get-parent-index custom-start-index))
            (get-parent-index custom-start-index))
     heap)))

(defn heapify-down
  ([heap compare]
   (heapify-down heap compare 0))
  ([heap compare custom-start-index]
   (if (has-left-child custom-start-index)
     (let [next-index (if (and (has-right-child heap custom-start-index)
                               (compare :less-then
                                        (right-child custom-start-index)
                                        (left-child custom-start-index)))
                        (get-right-child-index heap custom-start-index)
                        (get-left-child-index heap custom-start-index))]
       (if (compare :less-then
                    (nthh heap custom-start-index)
                    (nthh heap next-index))
         heap
         (recur (swap heap custom-start-index next-index)
                next-index)))
     heap)))

(defn peek [heap]
  (when (pos? (lenh heap))
    (first (cont heap))))

(defn poll [heap compare]
  (case (lenh heap)
    0 [heap nil]
    1 (update ((juxt butlast last) (cont heap))
              0 #(assoc-cont heap %))
    [(heapify-down (assoc-cont heap (move (cont heap) :last 0)) compare)
     (first (cont heap))]))

(defn add [heap item compare]
  (heapify-up (update-cont heap conj item) compare))

(defn find*
  [heap item compare custom-compare]
  (let [custom-compare (or custom-compare compare)
        len (lenh heap)]
    (loop [item-index 0
           found-item-indices []]
      (if (= item-index len)
        found-item-indices
        (recur (inc item-index)
               (if (custom-compare :equal
                                   item
                                   (nthh heap item-index))
                 (conj found-item-indices item-index)
                 found-item-indices))))))

(defn remove*
  [heap item compare custom-compare]
  (let [custom-compare (or custom-compare compare)
        number-of-items-to-remove (find* heap item compare custom-compare)]
    (loop [iteration 0
           heap-container (cont heap)]
      (if (= iteration number-of-items-to-remove)
        (assoc-cont heap heap-container)
        (recur (inc iteration)
               (let [index-to-remove (last (find* heap-container item compare custom-compare))]
                 (if (->> heap-container count dec (= index-to-remove))
                   (butlast heap-container)
                   (cont (let [heap-container (move heap-container :last index-to-remove)
                               parent-item (when (has-parent heap-container index-to-remove)
                                             (parent heap-container index-to-remove))
                               left-child-item (when (has-left-child heap-container
                                                                     index-to-remove)
                                                 (left-child heap-container index-to-remove))]
                           (if (->> index-to-remove
                                    (nth heap-container)
                                    (compare :less-then parent-item)
                                    (or parent-item)
                                    (and left-child-item))
                             (heapify-down (assoc-cont heap heap-container) compare index-to-remove)
                             (heapify-up (assoc-cont heap heap-container) compare index-to-remove)))))))))))

(defn empty? [heap]
  (zero? (lenh heap)))

(defn ->string [heap]
  (str (cont heap)))
