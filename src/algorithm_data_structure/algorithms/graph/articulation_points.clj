(ns algorithm-data-structure.algorithms.graph.articulation-points
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/articulation-points"
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.algorithms.graph.depth-first-search :as dfs]))

(defn- enter-vertex [visited-set discovery-time]
  (fn [{current-vertex :current-vertex
        previous-vertex :previous-vertices}]
    (swap! discovery-time inc)
    (swap! visited-set assoc
           (gv/get-key current-vertex)
           {:discovery-time @discovery-time
            :low-discovery-time @discovery-time
            :independent-children-count 0})
    (when previous-vertex
      (swap! visited-set update-in
             [(gv/get-key previous-vertex)
              :independent-children-count]
             #(if-not % 0 (inc %))))))

(defn- leave-vertex [visited-set articulation-points-set start-vertex]
  (fn [{current-vertex :current-vertex
        previous-vertex :previous-vertices}]
    (when previous-vertex
      (swap! visited-set update-in
             [(gv/get-key current-vertex) :low-discovery-time]
             (fn [low-discovery-time]
               (reduce (fn [acc n]
                         (let [neighbor-low-time
                               (get-in @visited-set
                                       [(gv/get-key n) :low-discovery-time])]
                           (if (< neighbor-low-time acc)
                             neighbor-low-time
                             acc)))
                       low-discovery-time
                       (filter #(not= (gv/get-key %)
                                      (gv/get-key previous-vertex))
                               (gv/get-neighbors current-vertex)))))
      (if (= previous-vertex start-vertex)
        (when (>= (get-in @visited-set
                          [(gv/get-key previous-vertex) :independent-children-count])
                  2)
          (swap! articulation-points-set assoc
                 (gv/get-key previous-vertex) previous-vertex))
        (let [current-discovery-time (get-in @visited-set
                                             [(gv/get-key current-vertex) :low-discovery-time])
              parent-discovery-time (get-in @visited-set
                                            [(gv/get-key previous-vertex) :discovery-time])]
          (when (<= parent-discovery-time current-discovery-time)
            (swap! articulation-points-set assoc
                   (gv/get-key previous-vertex) previous-vertex)))))))

(defn- allow-traversal [visited-set]
  (fn [{next-vertex :next-vertex}]
    (not (@visited-set (gv/get-key next-vertex)))))

(defn run [graph]
  (let [visited-set (atom {})
        articulation-points-set (atom {})
        discovery-time (atom 0)
        start-vertex (first (g/get-all-vertices graph))
        dsf-callbacks {:enter-vertex (enter-vertex visited-set discovery-time)
                       :leave-vertex (leave-vertex visited-set articulation-points-set start-vertex)
                       :allow-traversal (allow-traversal visited-set)}]
    (dfs/run graph start-vertex dsf-callbacks)
    @articulation-points-set))
