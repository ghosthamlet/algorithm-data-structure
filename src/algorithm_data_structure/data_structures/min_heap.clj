(ns algorithm-data-structure.data-structures.min-heap
  (:require [algorithm-data-structure.util :refer :all]))

(def ^:dynamic *compare*)
(def less-then)

;; TODO: use defrecored MinHeap
;;       implement nth count...
(defn create [comparator-function]
  {:heap-container []
   :compare comparator-function})

(defn cont [heap]
  (:heap-container heap))

(defn compare [heap]
  (:compare heap))

(defn lenh [heap]
  (count (cont heap)))

(defn nthh [heap n]
  (nth (cont heap) n))

(defn assoc-cont
  [heap & kvs]
  (assoc heap
         :heap-container (if (< (count kvs) 2)
                           (first kvs)
                           (apply assoc (cont heap) kvs))))

(defn update-cont [heap f & args]
  (apply update heap :heap-container f args))

(defn get-left-child-index [parent-index]
  (+ (* 2 parent-index) 1))

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
  ([heap]
   (heapify-up heap (dec (lenh heap))))
  ([heap custom-start-index]
   (if (and (has-parent custom-start-index)
            ((compare heap) less-then (nthh heap custom-start-index)
                       (parent custom-start-index)))
     (recur (swap heap
                  custom-start-index
                  (get-parent-index custom-start-index))
            (get-parent-index custom-start-index))
     heap)))

(defn heapify-down
  ([heap]
   (heapify-down heap 0))
  ([heap custom-start-index]
   (if (has-left-child custom-start-index)
     (let [next-index (if (and (has-right-child heap custom-start-index)
                               ((compare heap) less-then (right-child custom-start-index)
                                          (left-child custom-start-index)))
                        (get-right-child-index heap custom-start-index)
                        (get-left-child-index heap custom-start-index))]
       (if ((compare heap) less-then (nthh heap custom-start-index)
                      (nthh heap next-index))
         heap
         (recur (swap heap custom-start-index next-index)
                next-index)))
     heap)))

(defn peek [heap]
  (when (pos? (lenh heap))
    (first (cont heap))))

(defn poll [heap]
  (case (lenh heap)
    0 [heap nil]
    1 (update ((juxt butlast last) (cont heap))
              0 #(assoc-cont heap %))
    [(heapify-down (assoc-cont heap (move (cont heap) :last 0)))
     (first (cont heap))]))

(defn add [heap item]
  (heapify-up (update-cont heap conj item)))

(defn find*
  ([heap]
   (find* heap (compare heap)))
  ([heap custom-comparator]
   (let [len (lenh heap)]
   (loop [item-index 0
          found-item-indices []]
     (if (= item-index len)
       found-item-indices
       (recur (inc item-index)
              (if (custom-comparator item
                                     (nthh heap item-index))
                (conj found-item-indices item-index)
                found-item-indices)))))))

(defn remove*
  ([heap item]
   (remove* heap item (compare heap)))
  ([heap item custom-comparator]
   (let [number-of-items-to-remove (find* heap item)]
     (loop [iteration 0
            heap-container (cont heap)]
       (if (= iteration number-of-items-to-remove)
         (assoc-cont heap heap-container)
         (recur (inc iteration)
                (let [index-to-remove (last (find* heap-container item))]
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
                                     ((compare heap) less-then parent-item)
                                     (or parent-item)
                                     (and left-child-item))
                              (heapify-down (assoc-cont heap heap-container) index-to-remove)
                              (heapify-up (assoc-cont heap heap-container) index-to-remove))))))))))))

(defn empty? [heap]
  (zero? (lenh heap)))

(defn ->string [heap]
  (str (cont heap)))
