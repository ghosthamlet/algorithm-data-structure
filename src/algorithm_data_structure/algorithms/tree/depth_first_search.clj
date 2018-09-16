(ns algorithm-data-structure.algorithms.tree.depth-first-search)

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

(defn depth-first-search-recursive [node callbacks]
  ((:enter-node callbacks) node)
  (when (and (:left node)
             ((:allow-traversal callbacks) node (:left node)))
    (depth-first-search-recursive (:left node) callbacks))
  (when (and (:right node)
             ((:allow-traversal callbacks) node (:right node)))
    (depth-first-search-recursive (:right node) callbacks))
  ((:leave-node callbacks) node))

(defn run [root-node callbacks]
  (depth-first-search-recursive root-node (init-callback callbacks)))
