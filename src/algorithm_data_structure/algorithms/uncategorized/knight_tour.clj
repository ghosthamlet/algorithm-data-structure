(ns algorithm-data-structure.algorithms.uncategorized.knight-tour)

(defn get-possible-moves [chess-board position]
  (let [get-pm (fn [x x1 y y1 f1 f2]
                 [(f1 (position x) x1) (f2 (position y) y1)])
        possible-moves [(get-pm 0 1 1 2 - -) (get-pm 0 2 1 1 - -)
                        (get-pm 0 1 1 2 + -) (get-pm 0 2 1 1 + -)
                        (get-pm 0 2 1 1 - +) (get-pm 0 1 1 2 - +)
                        (get-pm 0 1 1 2 + +) (get-pm 0 2 1 1 + +)]
        board-size (count chess-board)]
    (filter (fn [move]
              (and (>= (move 0) 0)
                   (>= (move 1) 0)
                   (< (move 0) board-size)
                   (< (move 1) board-size)))
            possible-moves)))

(defn move-allowed? [chess-board move]
  (not= (get-in chess-board [(move 0) (move 1)]) 1))

(defn board-completely-visited [chess-board moves]
  (= (Math/pow (count chess-board) 2) (count moves)))

(defn knight-tour-recursive [chess-board moves]
  (let [current-chess-board chess-board]
    (if (board-completely-visited current-chess-board moves)
      [true moves]
      (loop [moves moves
             [current-move & rest-moves] (get-possible-moves current-chess-board
                                                             (moves (dec (count moves))))
             current-chess-board current-chess-board]
        (if-not current-move
          [false moves]
          (let [[ret moves current-chess-board]
                (if (move-allowed? current-chess-board current-move)
                  (let [moves (conj moves current-move)
                        current-chess-board (update-in current-chess-board
                                                       [(current-move 0) (current-move 1)]
                                                       1)
                        [ret moves] (knight-tour-recursive current-chess-board moves)]
                    (if ret
                      [ret moves current-chess-board]
                      [false (butlast moves) (update-in current-chess-board
                                                        [(current-move 0) (current-move 1)]
                                                        0)]))
                  [false moves current-chess-board])]
            (if ret
              [true moves]
              (recur moves
                     rest-moves
                     current-chess-board))))))))

(defn run [chess-board-size]
  (let [chess-board (map (fn [_] (repeat chess-board-size 0))
                         (repeat chess-board-size nil))
        moves []
        first-move [0 0]
        moves (conj first-move)
        chess-board (update-in chess-board [(first-move 0) (first-move 1)] 1)
        [solution-was-found moves] (knight-tour-recursive chess-board moves)]
    (if solution-was-found moves [])))
