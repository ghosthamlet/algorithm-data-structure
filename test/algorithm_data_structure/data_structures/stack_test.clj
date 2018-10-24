(ns algorithm-data-structure.data-structures.stack-test
  (:require [algorithm-data-structure.data-structures.stack :as s]
            [algorithm-data-structure.data-structures.linked-list :as ll]
            [clojure.test :refer :all]))

(def stack (s/create))

(deftest create-test
  (is (= {:linked-list (ll/create)}
         stack)))

(deftest push-test
  (is (= {:linked-list (-> (ll/create)
                           (ll/append 1))}
         (-> stack
             (s/push 1))))
  (is (= {:linked-list (-> (ll/create)
                           (ll/append 1)
                           (ll/append 0))}
         (-> stack
             (s/push 1)
             (s/push 0)))))

(deftest peek*-test
  (is (= nil
         (-> stack
             s/peek*)))
  (is (= 1
         (-> stack
             (s/push 1)
             s/peek*)))
  (is (= 0
         (-> stack
             (s/push 1)
             (s/push 0)
             s/peek*))))

(deftest pop*-test
  (is (= nil
         (-> stack
             s/pop*)))
  (is (= 1
         (-> stack
             (s/push 1)
             s/pop*)))
  (is (= 0
         (-> stack
             (s/push 1)
             (s/push 0)
             s/pop*))))

(deftest empty?*
  (is (= false
         (-> stack
             (s/push 1)
             s/empty?*)))
  (is (= true
         (-> stack
             s/empty?*))))

(deftest ->array-test
  (is (= []
         (-> stack
             s/->array)))
  (is (= [0 1]
         (-> stack
             (s/push 1)
             (s/push 0)
             s/->array))))

(deftest ->string-test
  (is (= "[]"
         (-> stack
             (s/->string nil))))
  (is (= "[\"1\" \"0\"]"
         (-> stack
             (s/push 1)
             (s/push 0)
             (s/->string nil)))))
