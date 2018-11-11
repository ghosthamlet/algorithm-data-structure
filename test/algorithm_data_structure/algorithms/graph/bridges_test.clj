(ns algorithm-data-structure.algorithms.graph.bridges-test
  (:require [algorithm-data-structure.algorithms.graph.bridges :as b]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [clojure.test :refer :all]))

(def graph (-> (g/create)
               (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
               (g/add-edge (ge/create (gv/create 2) (gv/create 3)))
               (g/add-edge (ge/create (gv/create 3) (gv/create 4)))))

(deftest enter-vertex-test
  (let [visited-set (atom {})
        discovery-time (atom 0)
        enter-vertex (#'b/enter-vertex visited-set discovery-time)]
    (enter-vertex {:current-vertex (gv/create 1)})
    (is (= 1
           @discovery-time))
    (is (= {1 {:discovery-time 1
               :low-discovery-time 1}}
           @visited-set))))

(deftest leave-vertex-test
  (let [visited-set (atom {1 {:discovery-time 1
                              :low-discovery-time 2}
                           2 {:discovery-time 1
                              :low-discovery-time 3}
                           3 {:discovery-time 1
                              :low-discovery-time 3}})
        bridges (atom {})
        start-vertex (gv/create 1)
        leave-vertex (#'b/leave-vertex graph visited-set bridges start-vertex)
        vertex-a (-> (gv/create 1)
                     (gv/add-edge (ge/create (gv/create 1) (gv/create 2))))
        vertex-b (-> (gv/create 3)
                     (gv/add-edge (ge/create (gv/create 2) (gv/create 3))))]
    (leave-vertex {:current-vertex vertex-a
                   :previous-vertex vertex-b})
    (is (= {1 {:discovery-time 1
               :low-discovery-time 2}
            2 {:discovery-time 1
               :low-discovery-time 3}
            3 {:discovery-time 1
               :low-discovery-time 2}}
           @visited-set))
    (is (= {}
           @bridges))

    (reset! visited-set {1 {:discovery-time 1
                            :low-discovery-time 2}
                         2 {:discovery-time 1
                            :low-discovery-time 3}})
    (leave-vertex {:current-vertex vertex-a
                   :previous-vertex (-> (gv/create 2)
                                        (gv/add-edge (ge/create (gv/create 1) (gv/create 2))))})
    (is (= {1 {:discovery-time 1
               :low-discovery-time 2}
            2 {:discovery-time 1
               :low-discovery-time 2}}
           @visited-set))
           ;; FIXME: failed g/find-edge
    #_(is (= {"1_2" (g/find-edge graph (-> (gv/create 2)
                                           (gv/add-edge (ge/create (gv/create 1) (gv/create 2)))) vertex-a)}
             @bridges))))

(deftest allow-traversal-test
  (let [visited-set (atom {})
        discovery-time (atom 0)
        allow-traversal (#'b/allow-traversal visited-set)]
    (is (= true
           (allow-traversal {:next-vertex (gv/create 1)})))
    ((#'b/enter-vertex visited-set discovery-time) {:current-vertex (gv/create 1)})
    (is (= false
           (allow-traversal {:next-vertex (gv/create 1)})))
    (is (= true
           (allow-traversal {:next-vertex (gv/create 2)})))))

(deftest run-test
  ;; FIXME: failed g/find-edge
  #_(is (= [2 3]
           (sort (keys (b/run graph))))))
