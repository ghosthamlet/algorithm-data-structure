(ns algorithm-data-structure.algorithms.string.knuth-morris-pratt)

(declare build-pattern-table)

(defn run [text word]
  (let [pattern-table (build-pattern-table word)]
    (loop [text-index 0
           word-index 0]
      (if (= text-index (count text))
        -1
        (cond
          (= (nth text text-index) (nth word word-index))
          (if (= word-index (dec (count word)))
            (inc (- text-index (count word)))
            (recur (inc text-index) (inc word-index)))
          (pos? word-index)
          (recur text-index (nth pattern-table (dec word-index)))
          :else
          (recur (inc text-index) 0))))))

(defn build-pattern-table [word]
  (loop [pattern-table [0]
         prefix-index 0
         suffix-index 1]
    (if (= suffix-index (count word))
      pattern-table
      (let [[pattern-table suffix-index prefix-index]
            (cond
              (= (nth word prefix-index) (nth word suffix-index))
              [(assoc pattern-table suffix-index (inc prefix-index))
               (inc suffix-index)
               (inc prefix-index)]
              (zero? prefix-index)
              [(assoc pattern-table suffix-index 0)
               (inc suffix-index)
               prefix-index]
              :else
              [pattern-table suffix-index prefix-index])]
        (recur pattern-table
               suffix-index
               prefix-index)))))
