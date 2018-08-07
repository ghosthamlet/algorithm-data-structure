(ns algorithm-data-structure.algorithms.string.z-algorithm)

(def separator "$")

(defn- build-z-array [z-string]
  (let [zlen (count z-string)
        z-array (atom (repeat zlen 0))
        z-box-left-index (atom 0)
        z-box-right-index (atom 0)
        z-box-shift (atom 0)]
    (doseq [char-index (range 1 zlen)]
      (if (> char-index @z-box-right-index)
        (do
          (reset! z-box-left-index char-index)
          (reset! z-box-right-index char-index)
          (while (and (< @z-box-right-index zlen)
                      (= (nth z-string (- @z-box-right-index @z-box-left-index))
                         (nth z-string @z-box-right-index)))
            (swap! z-box-right-index inc))
          (swap! z-array assoc char-index
                 (- @z-box-right-index @z-box-left-index))
          (swap! z-box-right-index dec))
        (do
          (reset! z-box-shift (- char-index @z-box-left-index))
          (if (< (nth @z-array @z-box-shift)
                 (inc (- @z-box-right-index char-index)))
            (swap! z-array assoc char-index
                   (nth @z-array @z-box-shift))
            (do
              (reset! z-box-right-index char-index)
              (while (and (< @z-box-right-index zlen)
                          (= (nth z-string (- @z-box-right-index @z-box-left-index))
                             (nth z-string @z-box-right-index)))
                (swap! z-box-right-index inc))
              (swap! z-array assoc char-index
                     (- @z-box-right-index @z-box-left-index))
              (swap! z-box-right-index dec))))))
    @z-array))

(defn run [text word]
  (let [z-string (str word separator text)
        z-array (build-z-array z-string)
        zlen (count z-array)
        wlen (count word)
        slen (count separator)]
    (loop [char-index 1
           word-positions []]
      (if (= char-index zlen)
        word-positions
        (recur (inc char-index)
               (if (= (nth z-array char-index) wlen)
                 (conj word-positions
                       (- char-index wlen slen))
                 word-positions))))))
