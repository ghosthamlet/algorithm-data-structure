(ns algorithm-data-structure.algorithms.graph.travelling-salesman
  (:require [algorithm-data-structure.data-structures.graph :as ggraph]
            [algorithm-data-structure.data-structures.graph-vertex :as gvertex]
            [algorithm-data-structure.util :refer :all]))

(defn find-all-paths
  ([start-vertex] (find-all-paths start-vertex [] []))
  ([start-vertex paths path]
   (let [current-path (conj path start-vertex)
         visited-set (reduce #(assoc %1 (gvertex/get-key %2) %2)
                             {} current-path)
         unvisited-neighbors (filter #(->> % gvertex/get-key (contains? visited-set) not)
                                     (gvertex/get-neighbors start-vertex))]
     (let [len (count unvisited-neighbors)]
       (if (zero? len)
         (conj paths current-path)
         (loop [neighbor-index 0
                paths paths]
           (if (= neighbor-index len)
             paths
             (recur (inc neighbor-index)
                    (find-all-paths (unvisited-neighbors neighbor-index)
                                    paths
                                    current-path)))))))))

(defn get-cycle-weight [adjacency-matrix vertices-indices cycle]
  (let [len (count cycle)
        idx-f #(->> % cycle gvertex/get-key vertices-indices)]
    (loop [cycle-index 1
           weight 0]
      (if (= cycle-index len)
        weight
        (recur (inc cycle-index)
               (+ weight
                  (get-in adjacency-matrix
                          [(idx-f (dec cycle-index))
                           (idx-f cycle-index)])))))))

(defn includes [vs v]
  (some #(= (gvertex/get-key v) (gvertex/get-key %)) vs))

(defn run [ggraph]
  (let [start-vertex (first (ggraph/get-all-vertices ggraph))
        all-possible-cycles (filter #(includes (->> % count dec % gvertex/get-neighbors) start-vertex)
                                    (find-all-paths start-vertex))
        len (count all-possible-cycles)
        adjacency-matrix (ggraph/get-adjacency-matrix ggraph)
        vertices-indices (ggraph/get-all-vertices ggraph)]
    (loop [cycle-index 0
           salesman-path []
           salesman-path-weight nil]
      (if (= cycle-index len)
        salesman-path
        (let [current-cycle (all-possible-cycles cycle-index)
              current-cycle-weight (get-cycle-weight adjacency-matrix
                                                     vertices-indices
                                                     current-cycle)
              [salesman-path salesman-path-weight]
              (if (or (nil? salesman-path-weight)
                      (< current-cycle-weight salesman-path-weight)
                      [current-cycle current-cycle-weight]
                      [salesman-path salesman-path-weight]))]
          (recur (inc cycle-index)
                 salesman-path
                 salesman-path-weight))))))
