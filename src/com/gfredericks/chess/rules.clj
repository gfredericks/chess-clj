(ns com.gfredericks.chess.rules
  "Which moves are legal and such.

  A move is just a [from-square to-square]."
  (:require [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.position] ;; need this for data readers
            [com.gfredericks.chess.squares :as sq]))

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

(defn ^:private move?
  "Returns true if arg is a valid move data structure."
  [x]
  (and (vector? x)
       (number? (first x))
       (number? (second x))
       (sq/square? (first x))
       (sq/square? (second x))
       (or (= 2 (count x))
           (and (= 3 (count x))
                (keyword? (last x))))))

(defn ^:private sqs-in-dir
  "Takes a square and a [dcol drow]"
  [sq [dcol drow]]
  (->> (iterate #(sq/translate % drow dcol) sq)
       (rest)
       (take-while identity)))

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
       (map (fn [[dcol drow]]
              (sq/translate sq drow dcol)))
       (filter identity)
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
  [board sq color]
  (let [dir (pawn-direction color)
        forward (sq/translate-row sq dir)
        jump (sq/translate-row forward dir)
        attack-left (sq/translate sq dir -1)
        attack-right (sq/translate sq dir 1)
        opponent (other-color color)

        applicable-moves
        (remove nil?
                [(if (= :_ (board/get board forward))
                   [sq forward])
                 (if (and jump
                          (= (sq/row sq) (pawn-start-row color))
                          (= :_ (board/get board forward))
                          (= :_ (board/get board jump)))
                   [sq jump])
                 (if (and attack-left
                          (= opponent (board/color-at board attack-left)))
                   [sq attack-left])
                 (if (and attack-right
                          (= opponent (board/color-at board attack-right)))
                   [sq attack-right])])]
    (if (= (pawn-penultimate-row color) (sq/row sq))
      (for [[from to] applicable-moves
            promote-piece (case color :white [:Q :R :B :N] :black [:q :r :b :n])]
        [from to promote-piece])
      applicable-moves)))

(defn normal-moves-for-piece
  [board piece color sq]
  ((case (board/piece-type piece)
     :king normal-king-moves
     :queen normal-queen-moves
     :rook normal-rook-moves
     :bishop normal-bishop-moves
     :knight normal-knight-moves
     :pawn normal-pawn-moves)
   board
   sq
   color))

(defn normal-moves
  [board color-to-move]
  (for [[sq p] (board/piece-placements board)
        :when (= color-to-move (board/piece-color p))
        mv (normal-moves-for-piece board p color-to-move sq)]
    mv))

(defn attacks?
  "Returns true if the given color is attacking the given square."
  [board attacking-color square]
  (->> (normal-moves board attacking-color)
       (some (fn [[from-square to-square]]
               (and (= to-square square)
                    ;; not a forward pawn move
                    (not (and (= :pawn (board/piece-type (board/get board from-square)))
                              (= (sq/col from-square) (sq/col to-square)))))))))

(defn castling-moves
  [board turn {:keys [king queen]}]
  (let [castling-row (case turn :white 0 :black 7)
        king-square (sq/square 4 castling-row)
        queen-hop-square (sq/square 3 castling-row)
        king-hop-square (sq/square 5 castling-row)
        attack-free? #(not (attacks? board (other-color turn) %))]
    (if (attack-free? king-square)
      (filter identity
              [(and queen
                    (= :_ (board/get board queen-hop-square))
                    (attack-free? queen-hop-square)
                    [king-square (sq/square 2 castling-row)])
               (and king
                    (= :_ (board/get board king-hop-square))
                    (attack-free? king-hop-square)
                    [king-square (sq/square 6 castling-row)])]))))

(defn en-passant-moves
  [board turn en-passant-square]
  (if en-passant-square
    (let [row (sq/row en-passant-square)
          col (sq/col en-passant-square)
          anti-pawn-dir (case turn :white -1 :black 1)
          left-sq (sq/translate en-passant-square anti-pawn-dir -1)

          [left-type left-color]
          (if left-sq (board/piece-info (board/get board left-sq)))

          right-sq (sq/translate en-passant-square anti-pawn-dir 1)

          [right-type right-color]
          (if right-sq (board/piece-info (board/get board right-sq)))]
      (filter identity
              [(and left-sq
                    (= :pawn (board/piece-type (board/get board left-sq)))
                    (= turn (board/piece-color (board/get board left-sq)))
                    [left-sq en-passant-square])
               (and right-sq
                    (= :pawn (board/piece-type (board/get board right-sq)))
                    (= turn (board/piece-color (board/get board right-sq)))
                    [right-sq en-passant-square])]))))

(defn make-move
  "Woah man is this gonna be a workhorse."
  [{:keys [board turn en-passant castling] :as pos} [from-square to-square promotion]]
  (let [board' (-> board
                   (board/set from-square :_)
                   (board/set to-square (or promotion (board/get board from-square))))
        piece-moved (board/piece-type (board/get board from-square))
        frow (sq/row from-square)
        fcol (sq/col from-square)
        trow (sq/row to-square)
        tcol (sq/col to-square)]
    (-> pos
        (assoc :board board'
               ;; set en-passant on pawn jump
               :en-passant (if (and (= :pawn piece-moved)
                                    (#{2 -2} (- frow trow)))
                             (sq/square fcol (/ (+ frow trow) 2)))
               :turn (other-color turn))
        ;; TODO: this is wrong; we only inc on non-captures/pawn-moves
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
                                   (sq/square (if left? 0 7) frow)

                                   new-rook-square
                                   (sq/square (if left? 3 5) frow)]
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
                                   capture-square (sq/square tcol (- trow pawn-dir))]
                               (board/set board' capture-square :_))))

                ;; set castling on king/rook moves
                (= :king piece-moved)
                (assoc-in [:castling turn] {:king false :queen false})

                (= :rook piece-moved)
                (cond->
                 (= from-square (sq/square 0 (case turn :white 0 :black 7)))
                 (assoc-in [:castling turn :queen] false)
                 (= from-square (sq/square 7 (case turn :white 0 :black 7)))
                 (assoc-in [:castling turn :king] false))))))

(defn self-checking-move?
  "Returns true if the move puts the moving player in check (and is
  thus illegal)."
  [pos move]
  {:pre [(move? move)]}
  (let [board (-> pos (make-move move) (:board))
        king-square (->> sq/all-squares
                         (filter #(= [:king (:turn pos)]
                                     (board/piece-info (board/get board %))))
                         (first))]
    (attacks? board (other-color (:turn pos)) king-square)))

(defn moves
  "Returns a list of all the legal moves from this position, ignoring
  the positions' half-move attribute."
  [{:keys [board turn castling en-passant] :as pos}]
  (->> (concat (normal-moves board turn)
               (castling-moves board turn (castling turn))
               (en-passant-moves board turn en-passant))
       (remove #(self-checking-move? pos %))))

(defn legal-move?
  [pos move]
  (boolean (some #{move} (moves pos))))

(defn player-to-move-in-check?
  [{:keys [turn board]}]
  (let [player's-king (->> (board/piece-placements board)
                           (filter (fn [[sq p]]
                                     (= [:king turn] (board/piece-info p))))
                           (ffirst))]
    (attacks? board (other-color turn) player's-king)))

(defn position-status
  "Returns one of #{:checkmate :stalemate :ongoing}."
  [pos]
  (if (empty? (moves pos))
    (if (player-to-move-in-check? pos)
      :checkmate
      :stalemate)
    :ongoing))
