(ns algorithm-data-structure.algorithms.graph.dijkstra-test
  (:require [algorithm-data-structure.algorithms.graph.dijkstra :as d]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [clojure.test :refer :all]))

(def graph (-> (g/create)
               (g/add-edge (ge/create (gv/create 1) (gv/create 2) 2))
               (g/add-edge (ge/create (gv/create 2) (gv/create 3) 7))
               (g/add-edge (ge/create (gv/create 3) (gv/create 4) 6))
               (g/add-edge (ge/create (gv/create 4) (gv/create 5) 10))
               (g/add-edge (ge/create (gv/create 5) (gv/create 6) 5))
               (g/add-edge (ge/create (gv/create 6) (gv/create 1) 1))))

(deftest run-test
  (let [{:keys [distances previous-vertices]} (d/run graph (gv/create 1))]
    (is (= {1 0
            2 2
            3 9
            4 15
            5 6
            6 1}
           distances))
    (is (= [1 2 3 4 5 6]
           (-> previous-vertices
               keys
               sort)))
    (is (= nil
           (previous-vertices 1)))
    (is (= 1
           (gv/get-key (previous-vertices 2))))
    (is (= 2
           (gv/get-key (previous-vertices 3))))
    (is (= 3
           (gv/get-key (previous-vertices 4))))
    (is (= 6
           (gv/get-key (previous-vertices 5))))
    (is (= 1
           (gv/get-key (previous-vertices 6))))))

(deftest run-with-local-mutable-test
  (let [{:keys [distances previous-vertices]} (d/run-with-local-mutable graph (gv/create 1))]
    (is (= {1 0
            2 2
            3 9
            4 15
            5 6
            6 1}
           distances))
    (is (= [1 2 3 4 5 6]
           (-> previous-vertices
               keys
               sort)))
    (is (= nil
           (previous-vertices 1)))
    (is (= 1
           (gv/get-key (previous-vertices 2))))
    (is (= 2
           (gv/get-key (previous-vertices 3))))
    (is (= 3
           (gv/get-key (previous-vertices 4))))
    (is (= 6
           (gv/get-key (previous-vertices 5))))
    (is (= 1
           (gv/get-key (previous-vertices 6))))))
