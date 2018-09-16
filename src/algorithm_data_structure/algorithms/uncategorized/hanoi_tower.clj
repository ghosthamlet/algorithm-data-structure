(ns algorithm-data-structure.algorithms.uncategorized.hanoi-tower
  (:require [algorithm-data-structure.data-structures.stack :as s]))

(defn hanoi-tower-recursive [& {:keys [number-of-discs
                                       from-pole
                                       with-pole
                                       to-pole
                                       move-callback]}]
  (if (= 1 number-of-discs)
    (do (move-callback (s/peek from-pole)
                       (s/->array from-pole)
                       (s/->array to-pole))
        (s/push to-pole (s/pop from-pole)))
    (let [to-pole (hanoi-tower-recursive :number-of-discs (dec number-of-discs)
                                         :from-pole from-pole
                                         :with-pole to-pole
                                         :to-pole with-pole
                                         :move-callback move-callback)
          to-pole (hanoi-tower-recursive :number-of-discs 1
                                         :from-pole from-pole
                                         :with-pole with-pole
                                         :to-pole to-pole
                                         :move-callback move-callback)]
      (hanoi-tower-recursive :number-of-discs (dec number-of-discs)
                             :from-pole with-pole
                             :with-pole from-pole
                             :to-pole to-pole
                             :move-callback move-callback))))

(defn run [& {:keys [number-of-discs
                     from-pole
                     with-pole
                     to-pole
                     move-callback]
              :or {from-pole (s/create)
                   with-pole (s/create)
                   to-pole (s/create)}}]
  (hanoi-tower-recursive :number-of-discs number-of-discs
                         :from-pole (reduce s/push
                                            from-pole (range number-of-discs 0 -1))
                         :with-pole with-pole
                         :to-pole to-pole
                         :move-callback move-callback))
