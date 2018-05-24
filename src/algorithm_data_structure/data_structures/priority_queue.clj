(ns algorithm-data-structure.data-structures.priority-queue
  (:require [algorithm-data-structure.data-structures.min-heap :as heap]
            [algorithm-data-structure.util :refer :all]))

(defn compare-value [a b]
  (if (= a b)
    0
    (if (< a b) -1 1)))

(defn compare-priority [queue]
  (fn [a b]
    (let [p (prior queue)]
      (compare-value (p a) (p b)))))

(defn create []
  (let [compare (compare-priority {})]
    {:heap (heap/create compare)
     :priorities {}
     :compare compare}))

(defn prior [queue]
  (:priorities queue))

(defn update-prior [queue f & args]
  (apply update queue :priorities f args))

;; XXX: before every heap/action,
;;      we have to assoc-compare to update queue in compare-priority
(defn assoc-compare
  ([queue]
   (assoc-compare queue (compare-priority queue)))
  ([queue f]
   (-> queue
       (assoc-in
        [:heap :compare] f)
       (assoc :compare f))))

(defn add
  ([queue item]
   (add queue item 0))
  ([queue item priority]
   (-> queue
       (assoc-in
        [:priorities item] priority)
       assoc-compare
       (update
        :heap heap/add item))))

(defn remove* [queue item custom-comparator]
  (-> queue
      assoc-compare
      :heap
      (heap/remove* item custom-comparator)
      ((partial assoc queue :heap))
      (update-prior dissoc item)))

(defn change-priority [queue item priority]
  (-> queue
      (remove* item compare-value)
      (add item priority)))

(defn find-by-value [queue item]
  (-> queue
      assoc-compare
      :heap
      (heap/find* compare-value)))

(defn has-value [queue item]
  (-> queue
      (find-by-value item)
      count
      pos?))
