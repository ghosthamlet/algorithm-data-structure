(ns algorithm-data-structure.algorithms.graph.bridges
  (:require [algorithm-data-structure.data-structures.graph :as ggraph]
            [algorithm-data-structure.data-structures.graph-vertex :as gvertex]
            [algorithm-data-structure.algorithms.graph.depth-first-search :as depth-first-search]))

(defn- enter-vertex [visited-set discovery-time]
  (fn [{current-vertex :current-vertex}]
    (swap! discovery-time inc)
    (swap! visited-set assoc
           (gvertex/get-key current-vertex)
           {:discovery-time discovery-time
            :low-discovery-time discovery-time})))

(defn- leave-vertex [visited-set bridges start-vertex]
  (fn [{current-vertex :current-vertex
        previous-vertex :previous-vertices}]
    (let [previous-key (gvertex/get-key previous-vertex)
          current-key (gvertex/get-key current-vertex)]
      (when previous-vertex
        (swap! visited-set update-in
               [current-key :low-discovery-time]
               (fn [low-discovery-time]
                 (reduce (fn [acc n]
                           (let [neighbor-low-time
                                 (get-in @visited-set
                                         [(gvertex/get-key n) :low-discovery-time])]
                             (if (< neighbor-low-time acc)
                               neighbor-low-time
                               acc)))
                         low-discovery-time
                         (filter #(not= (gvertex/get-key %) previous-key)
                                 (gvertex/get-neighbors current-vertex)))))
        (let [current-low-discovery-time (get-in @visited-set
                                                 [current-key :low-discovery-time])
              previous-low-discovery-time (get-in @visited-set
                                                  [previous-key :low-discovery-time])
              parent-discovery-time (get-in @visited-set
                                            [previous-key :discovery-time])]
          (when (< current-low-discovery-time previous-low-discovery-time)
            (swap! visited-set assoc-in
                   [previous-key :low-discovery-time] current-low-discovery-time))
          (when (< parent-discovery-time current-low-discovery-time)
            (let [bridge (ggraph/find-edge ggraph previous-vertex current-vertex)]
              (swap! bridges assoc
                     (gvertex/get-key bridge) bridge))))))))

(defn- allow-traversal [visited-set]
  (fn [{next-vertex :next-vertex}]
    (not (@visited-set (gvertex/get-key next-vertex)))))

(defn run [ggraph]
  (let [visited-set (atom {})
        bridges (atom {})
        discovery-time (atom 0)
        start-vertex (first (ggraph/get-all-vertices ggraph))
        dsf-callbacks {:enter-vertex (enter-vertex visited-set discovery-time)
                       :leave-vertex (leave-vertex visited-set bridges start-vertex)
                       :allow-traversal (allow-traversal visited-set)}]
    (depth-first-search/run ggraph start-vertex dsf-callbacks)
    @bridges))

