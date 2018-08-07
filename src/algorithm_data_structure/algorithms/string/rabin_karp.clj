(ns algorithm-data-structure.algorithms.string.rabin-karp)

(def prime 97)

(defn- hash-char [word char-index]
  (* (int (nth word char-index))
     (Math/pow prime char-index)))

(defn hash-word [word]
  (reduce #(+ %1 (hash-char word %2))
          0 (range (count word))))

(defn re-hash-word [prev-hash prev-word new-word]
  (let [new-word-last-index (dec (count new-word))]
    (+ (/ new-hash prime)
       (hash-char new-word new-word-last-index))))

(defn run [text word]
  (let [word-hash (hash-word word)
        prev-segment (atom nil)
        current-segment-hash (atom nil)
        wlen (count word)
        times (inc (- (count text) wlen))
        ret (atom -1)]
    (loop [char-index 0]
      (if (or (= char-index times) (not= @ret -1))
        @ret
        (let [current-segment (.substring text char-index (+ char-index wlen))]
          (if (nil? @current-segment-hash)
            (reset! current-segment-hash (hash-word current-segment))
            (swap! current-segment-hash #(re-hash-word % @prev-segment current-segment)))
          (reset! prev-segment @current-segment)
          (when (= word-hash @current-segment-hash)
            (let [number-of-matches (atom 0)]
              (dotimes [deep-char-index wlen]
                (when (= (nth word deep-char-index)
                         (nth text (+ char-index deep-char-index)))
                  (swap! number-of-matches inc)))
              (when (= @number-of-matches wlen)
                (reset! ret char-index))))
          (recur [(inc char-index)]))))))
