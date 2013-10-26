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

(defn attacks?
  "Returns true if the given color is attacking the given square."
  [board attacking-color square]
  (->> (normal-moves board attacking-color)
       (some (fn [[from-square to-square]]
               (and (= to-square square)
                    ;; not a forward pawn move
                    (not (and (= :pawn (board/piece-type (board/get board from-square)))
                              (= (second from-square) (second to-square)))))))))

(defn castling-moves
  [board turn {:keys [king queen]}]
  (let [castling-row (case turn :white 0 :black 7)
        king-square [castling-row 4]
        queen-rook-square [castling-row 0]
        king-rook-square [castling-row 7]
        attack-free? #(not (attacks? board (other-color turn) %))]
    (if (attack-free? king-square)
      (remove nil?
              [(and queen
                    (attack-free? [castling-row 3])
                    [king-square [castling-row 2]])
               (and king
                    (attack-free? [castling-row 5])
                    [king-square [castling-row 6]])]))))

(defn en-passant-moves
  [board turn en-passant-square]
  (if-let [[row col] en-passant-square]
    (let [pawn-dir (case turn :white 1 :black -1)
          [row col] en-passant-square
          left-sq [(- row pawn-dir) (dec col)]
          [left-type left-color] (board/piece-info (board/get board left-sq))
          right-sq [(- row pawn-dir) (inc col)]
          [right-type right-color] (board/piece-info (board/get board right-sq))]
      (remove nil?
              [(and (square? left-sq)
                    (= :pawn left-type)
                    (= turn left-color)
                    [left-sq en-passant-square])
               (and (square? right-sq)
                    (= :pawn right-type)
                    (= turn right-color)
                    [right-sq en-passant-square])]))))

(defn make-move
  "Woah man is this gonna be a workhorse."
  [{:keys [board turn en-passant castling] :as pos} [from-square to-square promotion]]
  (let [board' (-> board
                   (board/set from-square :_)
                   (board/set to-square (or promotion (board/get board from-square))))
        piece-moved (board/piece-type (board/get board from-square))
        [frow fcol] from-square
        [trow tcol] to-square]
    (-> pos
        (assoc :board board'
               ;; set en-passant on pawn jump
               :en-passant (if (and (= :pawn piece-moved)
                                    (#{2 -2} (- frow trow)))
                             [(/ (+ frow trow) 2) fcol])
               :turn (other-color turn))
        (update-in [:half-move] inc)
        (cond-> (= turn :black)
                (update-in [:full-move] inc)

                ;; move rook on castling moves
                (and (= :king piece-moved)
                     (#{2 -2} (- fcol tcol)))
                (update-in [:board]
                           (fn [board']
                             (let [left? (= 2 tcol)
                                   old-rook-square
                                   [frow (if left? 0 7)]
                                   new-rook-square
                                   [frow (if left? 3 5)]]
                               (-> board'
                                   (board/set old-rook-square :_)
                                   (board/set new-rook-square
                                              (board/get board' old-rook-square))))))

                ;; capture on en-passant moves
                (and (= en-passant to-square)
                     (= :pawn piece-moved))
                (update-in [:board]
                           (fn [board']
                             (let [pawn-dir (case turn :white 1 :black -1)
                                   capture-square [(- trow pawn-dir) tcol]]
                               (board/set board' capture-square :_))))

                ;; set castling on king/rook moves
                (= :king piece-moved)
                (assoc-in [:castling turn] {:king false :queen false})

                (= :rook piece-moved)
                (cond->
                 (= from-square [(case turn :white 0 :black 7) 0])
                 (assoc-in [:castling turn :queen] false)
                 (= from-square [(case turn :white 0 :black 7) 7])
                 (assoc-in [:castling turn :king] false))))))

(defn self-checking-move?
  "Returns true if the move puts the moving player in check (and is
  thus illegal)."
  [pos move]
  (let [board (-> pos (make-move move) (:board))
        king-square (->> board/all-squares
                         (filter #(= [:king (:turn pos)]
                                     (board/piece-info (board/get board %))))
                         (first))]
    (attacks? board (other-color (:turn pos)) king-square)))

(defn moves
  "Returns a list of all the legal moves from this position, ignoring
  the positions' half-move and full-move."
  [{:keys [board turn castling en-passant] :as pos}]
  (->> (concat (normal-moves board turn)
               (castling-moves board turn (castling turn))
               (en-passant-moves board turn en-passant))
       (remove #(self-checking-move? pos %))))
