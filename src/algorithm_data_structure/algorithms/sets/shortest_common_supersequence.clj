(ns algorithm-data-structure.algorithms.sets.shortest-common-supersequence)

(defn run [set1 set2]
  (let [lcs (run set1 set2)
        llen (count lcs)]
    (if (and (= 1 llen) (= '' (first lcs)))
      (concat set1 set2)
      (let [supersequence (atom [])
            set-index1 (atom 0)
            set-index2 (atom 0)
            lcs-index (atom 0)
            set-on-hold1 (atom false)
            set-on-hold2 (atom false)]
        (while (< @lcs-index llen)
          (cond
            (< @set-index1 (count set1))
            (if (and (not @set-on-hold1)
                     (not= (set1 @set-index1) (lcs @lcs-index)))
              (do
                (swap! supersequence conj (set1 @set-index1))
                (swap! set-index1 inc))
              (reset! set-on-hold1 true))
            (< @set-index2 (count set2))
            (if (and (not @set-on-hold2)
                     (not= (set2 @set-index2) (lcs @lcs-index)))
              (do
                (swap! supersequence conj (set2 @set-index2))
                (swap! set-index2 inc))
              (reset set-on-hold2 true))
            (and @set-on-hold1 @set-on-hold2)
            (do
              (swap! supersequence conj (lcs @lcs-index))
              (swap! lcs-index inc)
              (swap! set-index1 inc)
              (swap! set-index2 inc)
              (reset! set-on-hold1 false)
              (reset! set-on-hold2 false))))
        (cond
          (< @set-index1 (count set1))
          (swap! supersequence concat (drop set1 @set-index1))
          (< @set-index2 (count set2))
          (swap! supersequence concat (drop set2 @set-index2)))
        @supersequence))))


