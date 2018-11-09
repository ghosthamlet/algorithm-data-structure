(ns algorithm-data-structure.algorithms.graph.depth-first-search-test
  (:require [algorithm-data-structure.algorithms.graph.depth-first-search :as dfs]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [clojure.test :refer :all]))

(deftest allow-traversal-fn-test
  (let [allow-traversal (dfs/allow-traversal-fn)]
    (is (= true
           (allow-traversal {:next-vertex (gv/create 1)}))))
  (let [allow-traversal (dfs/allow-traversal-fn)]
    (is (= true
           (allow-traversal {:next-vertex (gv/create 1)})))
    (is (= false
           (allow-traversal {:next-vertex (gv/create 1)})))
    (is (= true
           (allow-traversal {:next-vertex (gv/create 2)})))))

(deftest run-test
  (let [graph (-> (g/create)
                  (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
                  (g/add-edge (ge/create (gv/create 2) (gv/create 3)))
                  (g/add-edge (ge/create (gv/create 3) (gv/create 4))))
        get-vertex #(-> graph
                        (g/get-vertex-by-key (-> (gv/create %) gv/get-key)))
        vertex (get-vertex 1)
        enter-ret (atom [])
        leave-ret (atom [])
        f-fn (fn [ret]
               (fn [{:keys [current-vertex previous-vertex]}]
                 (swap! ret conj (:value current-vertex))))]
    (-> graph
        (dfs/run vertex
          :enter-vertex (f-fn enter-ret)
          :leave-vertex (f-fn leave-ret)))
    (is (= [1 2 3 4]
           @enter-ret))
    (is (= [1 2 3 4]
           @leave-ret))

    (reset! enter-ret [])
    (reset! leave-ret [])
    (-> graph
        (dfs/run vertex
          :enter-vertex (f-fn enter-ret)
          :leave-vertex (f-fn leave-ret)
          :allow-traversal (let [seen (atom {})]
                             (fn [{next-vertex :next-vertex}]
                               (let [k (gv/get-key next-vertex)]
                                 (if (or (@seen k) (> (:value next-vertex) 2))
                                   false
                                   (do (swap! seen assoc k true)
                                       true)))))))
    (is (= [1 2]
           @enter-ret))
    (is (= [1 2]
           @leave-ret))))
