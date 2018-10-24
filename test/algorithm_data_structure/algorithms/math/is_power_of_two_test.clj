(ns algorithm-data-structure.algorithms.math.is-power-of-two-test
  (:require [algorithm-data-structure.algorithms.math.is-power-of-two :as ipot]
            [clojure.test :refer :all]))

(deftest run-test
  (is (= false
         (ipot/run 0)))
  (is (= true
         (ipot/run 1)))
  (is (= true
         (ipot/run 2)))
  (is (= false
         (ipot/run 3)))
  (is (= true
         (ipot/run 16))))
