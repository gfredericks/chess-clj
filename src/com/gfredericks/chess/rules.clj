(ns com.gfredericks.chess.rules
  "Which moves are legal and such.

  A move is just a [from-square to-square]."
  (:require [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.position :refer [get-piece set-piece]]))

(def other-color {:white :black, :black :white})

;; movements

(def rectilinear-movements
  [[0 1] [1 0] [0 -1] [-1 0]])

(def diagonal-movements
  [[1 1] [1 -1] [-1 1] [-1 -1]])

(def all-standard-movements
  (concat rectilinear-movements diagonal-movements))

(def knight-moves
  [[2 1] [1 2] [-2 1] [-1 2] [2 -1] [1 -2] [-2 -1] [-1 -2]])

(defn square+
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn square?
  [[row col]]
  (and (<= 0 row 7)
       (<= 0 col 7)))

(defn ^:private sqs-in-dir
  "Takes a square and a [dx dy]"
  [sq dir]
  (->> (iterate (partial square+ dir) sq)
       (rest)
       (take-while square?)))

(defn ray-moves
  [directions board sq its-color]
  (for [dir directions
        :let [sqs (sqs-in-dir sq dir)
              [blanks more] (split-with #(= :_ (board/get board %)) sqs)
              move-tos (cond-> blanks
                               (if-let [sq (first more)]
                                 (not= its-color (board/color-at board sq)))
                               (conj (first more)))]
        move-to move-tos]
    [sq move-to]))

(defn king-and-knight-moves
  [dirs board sq color]
  (->> dirs
       (map (partial square+ sq))
       (filter square?)
       (remove #(= color (board/color-at board %)))
       (map #(vector sq %))))

(def normal-king-moves
  (partial king-and-knight-moves all-standard-movements))
(def normal-queen-moves
  (partial ray-moves all-standard-movements))
(def normal-rook-moves
  (partial ray-moves rectilinear-movements))
(def normal-bishop-moves
  (partial ray-moves diagonal-movements))
(def normal-knight-moves
  (partial king-and-knight-moves knight-moves))

(def pawn-start-row {:white 1, :black 6})
(def pawn-direction {:white 1, :black -1})
(def pawn-penultimate-row {:white 6, :black 1})
(defn normal-pawn-moves
  [board [row col :as sq] color]
  (let [dir (pawn-direction color)
        +rowdir (+ row dir)
        forward [+rowdir col]
        jump [(+ +rowdir dir) col]
        attack-left [+rowdir (dec col)]
        attack-right [+rowdir (inc col)]
        opponent (other-color color)

        applicable-moves
        (remove nil?
                [(if (= :_ (board/get board forward))
                   [sq forward])
                 (if (and (square? jump)
                          (= row (pawn-start-row color))
                          (= :_ (board/get board forward))
                          (= :_ (board/get board jump)))
                   [sq jump])
                 (if (and (square? attack-left)
                          (= opponent (board/color-at board attack-left)))
                   [sq attack-left])
                 (if (and (square? attack-right)
                          (= opponent (board/color-at board attack-right)))
                   [sq attack-right])])]
    (if (= (pawn-penultimate-row color) row)
      (for [[from to] applicable-moves
            promote-piece (case color :white [:Q :R :B :N] :black [:q :r :b :n])]
        [from to promote-piece])
      applicable-moves)))

(defn normal-moves-for-piece
  [board sq]
  (let [[type color] (-> board
                         (board/get sq)
                         (board/piece-info))]
    ((case type
       :king normal-king-moves
       :queen normal-queen-moves
       :rook normal-rook-moves
       :bishop normal-bishop-moves
       :knight normal-knight-moves
       :pawn normal-pawn-moves)
     board
     sq
     color)))

(defn normal-moves
  [board color-to-move]
  (for [sq board/all-squares
        :when (= color-to-move (board/color-at board sq))
        mv (normal-moves-for-piece board sq)]
    mv))

(defn moves
  "Returns a list of all the legal moves from this position, ignoring
  the positions' half-move and full-move."
  [{:keys [board turn castling en-passant]}]
  )
