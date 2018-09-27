(ns algorithm-data-structure.data-structures.priority-queue
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/priority-queue"
  (:require [algorithm-data-structure.data-structures.min-heap :as mh]
            [algorithm-data-structure.comparator :refer :all]
            [algorithm-data-structure.util :refer :all]))

(declare prior)

(defrecord PriorityQueue [priorities])

(defn create []
  (merge (PriorityQueue. {}) (mh/create)))

(defmethod compare-value PriorityQueue [self a b]
  (let [p (prior self)]
    (compare-default self (p a) (p b))))

(defn prior [self]
  (:priorities self))

(defn update-prior [self f & args]
  (apply update self :priorities f args))

(defn poll [self]
  (mh/poll self))

(defn add
  ([self item]
   (add self item 0))
  ([self item priority]
   (-> self
       (assoc-in [:priorities item] priority)
       (mh/add item))))

(defn remove* [self item custom-comparator]
  (-> self
      (mh/remove* item custom-comparator)
      (update-prior dissoc item)))

(defn change-priority [self item priority]
  (-> self
      (remove* item compare-default)
      (add item priority)))

(defn find-by-value [self item]
  (-> self
      (mh/find* item compare-default)))

(defn has-value [self item]
  (-> self
      (find-by-value item)
      count
      pos?))

;; XXX: before every mh/action,
;;      we have to assoc-compare to update self in compare-priority-fn
;;      NO USED
#_(defn assoc-compare
    ([self]
     (assoc-compare self (compare-priority-fn self)))
    ([self f]
     (-> self
         (assoc-in
          [:mh :compare] f)
         (assoc :compare f))))

#_(defn- compare-priority-fn [self]
    (fn compare
      ([action a b]
       (compare-action compare action a b))
      ([a b]
       (let [p (prior self)]
         (compare-value (p a) (p b))))))

