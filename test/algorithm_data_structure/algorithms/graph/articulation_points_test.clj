(ns algorithm-data-structure.algorithms.graph.articulation-points-test
  (:require [algorithm-data-structure.algorithms.graph.articulation-points :as ap]
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
        enter-vertex (#'ap/enter-vertex visited-set discovery-time)]
    (enter-vertex {:current-vertex (gv/create 1)
                   :previous-vertex (gv/create 2)})
    (is (= 1
           @discovery-time))
    (is (= {1 {:discovery-time 1
               :low-discovery-time 1
               :independent-children-count 0}
            2 {:independent-children-count 0}}
           @visited-set))

    (enter-vertex {:current-vertex (gv/create 1)
                   :previous-vertex (gv/create 2)})
    (is (= 2
           @discovery-time))
    (is (= {1 {:discovery-time 2
               :low-discovery-time 2
               :independent-children-count 0}
            2 {:independent-children-count 1}}
           @visited-set))

    (reset! visited-set {})
    (reset! discovery-time 0)
    (enter-vertex {:current-vertex (gv/create 1)})
    (is (= 1
           @discovery-time))
    (is (= {1 {:discovery-time 1
               :low-discovery-time 1
               :independent-children-count 0}}
           @visited-set))))

(deftest leave-vertex-test
  (let [visited-set (atom {1 {:discovery-time 1
                              :low-discovery-time 1
                              :independent-children-count 0}})
        articulation-points-set (atom {})
        start-vertex (gv/create 1)
        leave-vertex (#'ap/leave-vertex visited-set articulation-points-set start-vertex)]
    (leave-vertex {:current-vertex (-> (gv/create 1)
                                       (gv/add-edge (ge/create (gv/create 1) (gv/create 2)))
                                       (gv/add-edge (ge/create (gv/create 1) (gv/create 3))))
                   :previous-vertex (gv/create 2)})
    (is (= {1 {:discovery-time 1
               :low-discovery-time 0
               :independent-children-count 0}}
           @visited-set))
    (is (= {2 (gv/create 2)}
           @articulation-points-set))

    (reset! visited-set {1 {:discovery-time 1
                            :low-discovery-time 1
                            :independent-children-count 2}
                         3 {:discovery-time 1
                            :low-discovery-time 0
                            :independent-children-count 0}})
    (reset! articulation-points-set {})
    (leave-vertex {:current-vertex (-> (gv/create 1)
                                       (gv/add-edge (ge/create (gv/create 1) (gv/create 2)))
                                       (gv/add-edge (ge/create (gv/create 1) (gv/create 3))))
                   :previous-vertex start-vertex})
    (is (= {1 {:discovery-time 1
               :low-discovery-time 0
               :independent-children-count 2}
            3 {:discovery-time 1
               :low-discovery-time 0
               :independent-children-count 0}}
           @visited-set))
    (is (= {1 start-vertex}
           @articulation-points-set))))

(deftest allow-traversal-test
  (let [visited-set (atom {})
        discovery-time (atom 0)
        allow-traversal (#'ap/allow-traversal visited-set)]
    (is (= true
           (allow-traversal {:next-vertex (gv/create 1)})))
    ((#'ap/enter-vertex visited-set discovery-time) {:current-vertex (gv/create 1)
                                                     :previous-vertex (gv/create 6)})
    (is (= false
           (allow-traversal {:next-vertex (gv/create 1)})))
    (is (= true
           (allow-traversal {:next-vertex (gv/create 2)})))))

(deftest run-test
  (is (= [2 3]
         (sort (keys (ap/run graph))))))
