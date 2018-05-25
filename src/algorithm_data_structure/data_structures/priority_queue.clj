(ns algorithm-data-structure.data-structures.priority-queue
  (:require [algorithm-data-structure.data-structures.min-heap :as heap]
            [algorithm-data-structure.util :refer :all]))

(defn- compare-action [f action a b]
  (case action
    :equal (zero? (f a b))
    :less-then (neg? (f a b))
    (throw (Exception. "no implemention"))))

(defn compare-value
  ([action a b]
   (compare-action compare-value action a b))
  ([a b]
   (if (= a b)
     0
     (if (< a b) -1 1))))

(defn compare-priority-fn [queue]
  (fn compare
    ([action a b]
     (compare-action compare action a b))
    ([a b]
     (let [p (prior queue)]
       (compare-value (p a) (p b))))))

(defn create []
  {:heap (heap/create)
   :priorities {}})

(defn prior [queue]
  (:priorities queue))

(defn update-prior [queue f & args]
  (apply update queue :priorities f args))

(defn add
  ([queue item]
   (add queue item 0))
  ([queue item priority]
   (-> queue
       (assoc-in
        [:priorities item] priority)
       (update
        :heap heap/add item (compare-priority-fn queue)))))

(defn remove* [queue item custom-comparator]
  (-> queue
      :heap
      (heap/remove* item (compare-priority-fn queue) custom-comparator)
      ((partial assoc queue :heap))
      (update-prior dissoc item)))

(defn change-priority [queue item priority]
  (-> queue
      (remove* item compare-value)
      (add item priority)))

(defn find-by-value [queue item]
  (-> queue
      :heap
      (heap/find* item (compare-priority-fn queue) compare-value)))

(defn has-value [queue item]
  (-> queue
      (find-by-value item)
      count
      pos?))

;; XXX: before every heap/action,
;;      we have to assoc-compare to update queue in compare-priority-fn
;;      NO USED
#_(defn assoc-compare
    ([queue]
     (assoc-compare queue (compare-priority-fn queue)))
    ([queue f]
     (-> queue
         (assoc-in
          [:heap :compare] f)
         (assoc :compare f))))
