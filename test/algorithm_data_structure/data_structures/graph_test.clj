(ns algorithm-data-structure.data-structures.graph-test
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(def graph (g/create))

(deftest create-test
  (is (= {:vertices {}
          :edges {}
          :directed? false}
         graph))
  (is (= {:vertices {}
          :edges {}
          :directed? true}
         (g/create true))))

(deftest add-vertex-test
  (is (= (-> {:vertices {1 (gv/create 1)
                         2 (gv/create 2)}
              :edges {}
              :directed? false}
             record->map)
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/add-vertex (gv/create 2))
             record->map))))

(deftest get-vertex-by-key-test
  (is (= (-> (gv/create 1)
             record->map)
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/get-vertex-by-key 1)
             record->map))))

(deftest assoc-vertex-by-key-test
  (is (= (-> {:vertices {1 (gv/create 1)}
              :edges {}
              :directed? false}
             record->map)
         (-> graph
             (g/assoc-vertex-by-key 1 (gv/create 1))
             record->map)))
  (is (= (-> {:vertices {1 (gv/create 2)}
              :edges {}
              :directed? false}
             record->map)
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/assoc-vertex-by-key 1 (gv/create 2))
             record->map))))

(deftest get-edge-vertex-key-test
  (is (= 1
         (-> (ge/create (gv/create 1) (gv/create 2))
             (g/get-edge-vertex-key :start-vertex))))
  (is (= 2
         (-> (ge/create (gv/create 1) (gv/create 2))
             (g/get-edge-vertex-key :end-vertex)))))

(deftest get-edge-vertex-test
  (is (= (-> (gv/create 2)
             record->map)
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/add-vertex (gv/create 2))
             (g/get-edge-vertex (ge/create (gv/create 1) (gv/create 2)) :end-vertex)
             record->map))))

(deftest add-edge-test
  (is (= (-> {:vertices {1 (-> (gv/create 1)
                               (gv/add-edge (ge/create (gv/create 1) (gv/create 2))))
                         2 (gv/create 2)}
              :edges {"1_2" (ge/create (gv/create 1) (gv/create 2))}
              :directed? true}
             record->map)
         (-> (g/create true)
             (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
             record->map)))
  (is (= (-> {:vertices {1 (-> (gv/create 1)
                               (gv/add-edge (ge/create (gv/create 1) (gv/create 2))))
                         2 (-> (gv/create 2)
                               (gv/add-edge (ge/create (gv/create 1) (gv/create 2))))}
              :edges {"1_2" (ge/create (gv/create 1) (gv/create 2))}
              :directed? false}
             record->map)
         (-> graph
             (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
             record->map)))
  (is (thrown? Exception
               (-> graph
                   (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
                   (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
                   record->map))))

(deftest delete-edge-test
  (is (thrown? Exception
               (-> graph
                   (g/delete-edge (ge/create (gv/create 1) (gv/create 2))))))
  (is (= (-> {:vertices {1 (gv/create 1)
                         2 (gv/create 2)}
              :edges {}
              :directed? false}
             record->map)
         (-> graph
             (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
             (g/delete-edge (ge/create (gv/create 1) (gv/create 2)))
             record->map))))

(deftest find-edge-test
  (is (= nil
         (-> graph
             (g/find-edge (gv/create 1) (gv/create 2)))))
  (is (= (ge/create (gv/create 1) (gv/create 2))
         (-> graph
             (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
             (g/find-edge (gv/create 1) (gv/create 2))))))

(deftest get-weight-test
  (is (= 0
         (-> graph
             g/get-weight))))

(deftest reverse-test
  (is (= graph
         (-> graph
             g/reverse)))
  (is (= (-> graph
             (g/add-edge (ge/create (gv/create 2) (gv/create 1))))
         (-> graph
             (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
             g/reverse))))

(deftest get-vertices-indices-test
  (is (= {}
         (-> graph
             g/get-vertices-indices)))
  (is (= {1 0
          2 1}
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/add-vertex (gv/create 2))
             g/get-vertices-indices))))


(deftest get-adjacency-matrix-test
  (is (= '()
         (-> graph
             g/get-adjacency-matrix)))
  (is (= '((2147483647 2147483647) (2147483647 2147483647))
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/add-vertex (gv/create 2))
             g/get-adjacency-matrix)))
  (is (= '((2147483647 0) (0 2147483647))
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/add-vertex (gv/create 2))
             (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
             g/get-adjacency-matrix))))

(deftest ->sting-test
  (is (= ""
         (-> graph
             g/->string)))
  (is (= "(1 2)"
         (-> graph
             (g/add-vertex (gv/create 1))
             (g/add-vertex (gv/create 2))
             g/->string))))
