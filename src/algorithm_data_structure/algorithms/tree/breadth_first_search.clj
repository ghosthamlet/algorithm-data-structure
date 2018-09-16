(ns algorithm-data-structure.algorithms.tree.breadth-first-search
  (:require [algorithm-data-structure.data-structures.queue :as q]))

(defn init-callback
  ([]) (init-callback {})
  ([callbacks]
   (let [stub-callback (fn [] {})
         default-allow-traveral (fn [] true)]
     {:allow-traversal (or (:allow-traversal callbacks)
                           default-allow-traveral)
      :enter-node (or (:enter-node callbacks)
                      stub-callback)
      :leave-node (or (:leave-node callbacks)
                      stub-callback)})))

(defn run [root-node original-callbacks]
  (let [callbacks (init-callback original-callbacks)
        node-queue (q/create)]
    (q/enqueue node-queue root-node)
    (loop [node-queue node-queue]
      (when-not (q/empty? node-queue)
        (let [[node-queue current-node] (q/dequeue node-queue)
              node-queue (if (and (:left current-node)
                                  ((:allow-traversal callbacks) current-node
                                   (:left current-node)))
                           (q/enqueue node-queue (:left current-node))
                           node-queue)
              node-queue (if (and (:right current-node)
                                  ((:allow-traversal callbacks) current-node
                                   (:right current-node)))
                           (q/enqueue node-queue (:right current-node))
                           node-queue)]
          ((:enter-node callbacks) current-node)
          ((:leave-node callbacks) current-node)
          (recur node-queue))))))
