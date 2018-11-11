(ns algorithm-data-structure.algorithms.graph.bridges
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/bridges"
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.algorithms.graph.depth-first-search :as dfs]))

(defn- enter-vertex [visited-set discovery-time]
  (fn [{current-vertex :current-vertex}]
    (swap! discovery-time inc)
    (swap! visited-set assoc
           (gv/get-key current-vertex)
           {:discovery-time @discovery-time
            :low-discovery-time @discovery-time})))

(defn- leave-vertex [graph visited-set bridges start-vertex]
  (fn [{current-vertex :current-vertex
        previous-vertex :previous-vertex}]
    (let [previous-key (gv/get-key previous-vertex)
          current-key (gv/get-key current-vertex)]
      (when previous-vertex
        (swap! visited-set update-in
               [current-key :low-discovery-time]
               (fn [low-discovery-time]
                 (reduce (fn [acc n]
                           (let [neighbor-low-time
                                 (get-in @visited-set
                                         [(gv/get-key n) :low-discovery-time]
                                         0)]
                             (if (< neighbor-low-time acc)
                               neighbor-low-time
                               acc)))
                         low-discovery-time
                         (filter #(not= (gv/get-key %) previous-key)
                                 (gv/get-neighbors current-vertex)))))
        (let [current-low-discovery-time (get-in @visited-set
                                                 [current-key :low-discovery-time]
                                                 0)
              previous-low-discovery-time (get-in @visited-set
                                                  [previous-key :low-discovery-time]
                                                  0)
              parent-discovery-time (get-in @visited-set
                                            [previous-key :discovery-time]
                                            0)]
          (when (< current-low-discovery-time previous-low-discovery-time)
            (swap! visited-set assoc-in
                   [previous-key :low-discovery-time] current-low-discovery-time))
          (when (< parent-discovery-time current-low-discovery-time)
            (when-let [bridge (g/find-edge graph previous-vertex current-vertex)]
              (swap! bridges assoc
                     (gv/get-key bridge) bridge))))))))

(defn- allow-traversal [visited-set]
  (fn [{next-vertex :next-vertex}]
    (not (@visited-set (gv/get-key next-vertex)))))

(defn run [graph]
  (let [visited-set (atom {})
        bridges (atom {})
        discovery-time (atom 0)
        start-vertex (first (g/get-all-vertices graph))]
    (dfs/run graph 
             start-vertex 
             :enter-vertex (enter-vertex visited-set discovery-time)
             :leave-vertex (leave-vertex graph visited-set bridges start-vertex)
             :allow-traversal (allow-traversal visited-set))
    @bridges))

