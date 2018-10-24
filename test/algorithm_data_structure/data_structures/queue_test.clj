(ns algorithm-data-structure.data-structures.queue-test
  (:require [algorithm-data-structure.data-structures.queue :as q]
            [algorithm-data-structure.data-structures.linked-list :as ll]
            [clojure.test :refer :all]))

(def queue (q/create))

(deftest create-test
  (is (= {:linked-list (ll/create)}
         queue)))

(deftest enqueue-test
  (is (= {:linked-list (-> (ll/create)
                           (ll/append 1))}
         (-> queue
             (q/enqueue 1))))
  (is (= {:linked-list (-> (ll/create)
                           (ll/append 1)
                           (ll/append 2))}
         (-> queue
             (q/enqueue 1)
             (q/enqueue 2)))))

(deftest empty?*-test
  (is (= false
         (-> queue
             (q/enqueue 1)
             q/empty?*)))
  (is (= true
         (-> queue
             q/empty?*))))

(deftest peek*-test
  (is (= nil
         (-> queue
             q/peek*)))
  (is (= 1
         (-> queue
             (q/enqueue 1)
             q/peek*)))
  (is (= 1
         (-> queue
             (q/enqueue 1)
             (q/enqueue 2)
             q/peek*))))

(deftest dequeue-test
  (is (= [queue nil]
         (-> queue
             q/dequeue)))
  (is (= [queue 1]
         (-> queue
             (q/enqueue 1)
             q/dequeue)))
  (is (= [(-> queue
              (q/enqueue 2))
          1]
         (-> queue
             (q/enqueue 1)
             (q/enqueue 2)
             q/dequeue))))

(deftest ->string-test
  (is (= "[]"
         (-> queue
             (q/->string nil))))
  (is (= "[\"1\" \"2\"]"
         (-> queue
             (q/enqueue 1)
             (q/enqueue 2)
             (q/->string nil)))))
