(ns com.gfredericks.chess.rules
  "Which moves are legal and such.

  A move is just a [from-square to-square]."
  (:require [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.moves :as moves]
            [com.gfredericks.chess.pieces :as pieces]
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

(defn ^:private sqs-in-dir
  "Takes a square and a [dcol drow]"
  [sq [dcol drow]]
  (->> (iterate #(sq/translate % drow dcol) sq)
       (rest)
       (take-while identity)))

(defn ray-moves
  [directions board sq its-color]
  (mapcat (fn [dir]
            (let [sqs (sqs-in-dir sq dir)
                  [blanks [maybe-piece]] (split-with #(= :_ (board/get board %)) sqs)]
              (cond-> (map (fn [sq']
                             (moves/->BasicMove sq sq'))
                           blanks)
                      (and maybe-piece (not (pieces/color? its-color
                                                           (board/get board maybe-piece))))
                      (conj (moves/->BasicCaptureMove
                             sq
                             maybe-piece
                             (board/get board maybe-piece))))))
          directions))

(defn king-and-knight-squares
  [dirs sq]
  (->> dirs
       (map (fn [[dcol drow]]
              (sq/translate sq drow dcol)))
       (filter identity)))

(defn king-and-knight-moves
  [dirs board sq color]
  (->> (king-and-knight-squares dirs sq)
       (map (juxt identity #(board/get board %)))
       (remove #(pieces/color? color (second %)))
       (map (fn [[sq' p]]
              (if (= :_ p)
                (moves/->BasicMove sq sq')
                (moves/->BasicCaptureMove sq sq' p))))))

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
        promoting? (= (pawn-penultimate-row color) (sq/row sq))
        promotingly #(map % (case color :white [:Q :R :B :N] :black [:q :r :b :n]))
        pawn (board/get board sq)]
    (remove nil?
            (apply concat
             [(if (= :_ (board/get board forward))
                (if promoting?
                  (promotingly #(moves/->PromotionMove sq forward pawn %))
                  [(moves/->PawnForwardMove sq forward)]))
              (if (and jump
                       (= (sq/row sq) (pawn-start-row color))
                       (= :_ (board/get board forward))
                       (= :_ (board/get board jump)))
                [(moves/->PawnForwardMove sq jump)])
              (if-let [p (and attack-left (board/get board attack-left))]
                (when (pieces/color? opponent p)
                  (if promoting?
                    (promotingly #(moves/->PromotionCapture sq attack-left pawn % p))
                    [(moves/->PawnCaptureMove sq attack-left p)])))
              (if-let [p (and attack-right (board/get board attack-right))]
                (when (pieces/color? opponent p)
                  (if promoting?
                    (promotingly #(moves/->PromotionCapture sq attack-right pawn % p))
                    [(moves/->PawnCaptureMove sq attack-right p)])))]))))

(defn normal-moves-for-piece
  [board piece color sq]
  ((case (pieces/piece-type piece)
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
        :when (pieces/color? color-to-move p)
        mv (normal-moves-for-piece board p color-to-move sq)]
    mv))

(defn attacks?
  "Returns true if the given color is attacking the given square."
  [board attacking-color square]
  ;; This is a bit haxy -- in order to correctly detect attacks
  ;; on blank squares by pawns (e.g., for castling), we make
  ;; sure the square isn't blank before calling normal-moves.
  (let [board' (board/set board square (case attacking-color
                                         :white :n
                                         :black :N))]
    (->> (normal-moves board' attacking-color)
         (some (fn [move] (= square (moves/attacking-square move)))))))

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
                    (moves/->CastlingMove (sq/square 0 castling-row)))
               (and king
                    (= :_ (board/get board king-hop-square))
                    (attack-free? king-hop-square)
                    (moves/->CastlingMove (sq/square 7 castling-row)))]))))

(defn en-passant-moves
  [board turn en-passant-square]
  (if en-passant-square
    (let [row (sq/row en-passant-square)
          col (sq/col en-passant-square)
          anti-pawn-dir (case turn :white -1 :black 1)
          capture-sq (sq/translate-row en-passant-square anti-pawn-dir)
          left-sq (sq/translate en-passant-square anti-pawn-dir -1)

          [left-type left-color]
          (if left-sq (pieces/piece-info (board/get board left-sq)))

          right-sq (sq/translate en-passant-square anti-pawn-dir 1)

          [right-type right-color]
          (if right-sq (pieces/piece-info (board/get board right-sq)))]
      (filter identity
              [(and left-sq
                    (pieces/pawn? (board/get board left-sq))
                    (= turn (pieces/piece-color (board/get board left-sq)))
                    (moves/->EnPassantMove left-sq
                                           en-passant-square
                                           capture-sq
                                           (board/get board capture-sq)))
               (and right-sq
                    (pieces/pawn? (board/get board right-sq))
                    (= turn (pieces/piece-color (board/get board right-sq)))
                    (moves/->EnPassantMove right-sq
                                           en-passant-square
                                           capture-sq
                                           (board/get board capture-sq)))]))))

(defn make-move-board
  [board move]
  ;; doesn't do a whole lot anymore
  (moves/apply-forward move board))

(defn make-move
  [{:keys [board turn half-move] :as pos} move]
  (-> pos
      (update-in [:board] make-move-board move)
      (assoc :en-passant (moves/en-passant-square move)
             :turn (other-color turn)
             :half-move (if (moves/progressive? move)
                          0
                          (inc half-move)))
      (update-in [:castling] moves/update-castling move)
      (cond-> (= turn :black)
              (update-in [:full-move] inc))))

(defn ^:private king-checker
  [sq]
  (let [sqs (set (king-and-knight-squares all-standard-movements sq))]
    (fn [board sq] (sqs sq))))

(defn ^:private knight-checker
  [sq]
  (let [sqs (set (king-and-knight-squares knight-moves sq))]
    (fn [board sq] (sqs sq))))

(def rowcol (juxt sq/row sq/col))
(defn dirtype
  "Returns [dcol drow] if the relationship between the squares is rectilinear
  or diagonal. Assumes they are not equal."
  [sq1 sq2]
  (let [row1 (sq/row sq1)
        row2 (sq/row sq2)
        col1 (sq/col sq1)
        col2 (sq/col sq2)
        d-row (- row2 row1)
        d-col (- col2 col1)
        abs #(* % (Long/signum %))]
    (if (or (= (abs d-row) (abs d-col))
            (zero? d-row)
            (zero? d-col))
      [(Long/signum d-col) (Long/signum d-row)])))

(defn ^:private check-dir
  [board sq1 sq2 dir]
  (loop [[sq & more] (sqs-in-dir sq1 dir)]
    (cond (= sq sq2) true
          (pieces/blank? (board/get board sq)) (recur more)
          :else false)))

(defn ^:private queen-checker
  [sq]
  (fn [board sq']
    (if-let [dir (dirtype sq sq')]
      (check-dir board sq sq' dir))))

(defn ^:private rook-checker
  [sq]
  (fn [board sq']
    (if-let [[dcol drow :as dir] (dirtype sq sq')]
      (if (or (zero? dcol) (zero? drow))
        (check-dir board sq sq' dir)))))

(defn ^:private bishop-checker
  [sq]
  (fn [board sq']
    (if-let [[dcol drow :as dir] (dirtype sq sq')]
      (if (not (or (zero? dcol) (zero? drow)))
        (check-dir board sq sq' dir)))))

(defn ^:private pawn-checker
  [sq piece-color]
  (let [drow (case piece-color :white 1 :black -1)
        sqs (->> [1 -1]
                 (map (fn [dcol] (sq/translate sq drow dcol)))
                 (filter identity)
                 (set))]
    (fn [board sq'] (sqs sq'))))

(defn ^:private check-checker-piece
  [board sq piece]
  (let [piece-type (pieces/piece-type piece)
        checker (if (= :pawn piece-type)
                  (pawn-checker sq (pieces/piece-color piece))
                  ((case piece-type
                       :king king-checker
                       :queen queen-checker
                       :rook rook-checker
                       :bishop bishop-checker
                       :knight knight-checker)
                   sq))]
    (fn [board' sq']
      ;; make sure the piece is still there; if not it has
      ;; presumably been captured
      (if (= piece (board/get board' sq))
        (checker board' sq')))))

(defn some-fn'
  "Like some-fn but does something sane for multiple-arg functions."
  [& fs]
  (fn [& args]
    (some #(apply % args) fs)))

(defn ^:private check-checker
  "Returns a function
    (fn [board' sq]) => bool
  that expects a board that is similar to the given board, such that
  none of the attacking pieces have moved (though they may have been
  captured), and returns a boolean indicating if the given square is
  under attack."
  [board attacking-player]
  (->> (board/piece-placements board)
       (filter #(pieces/color? attacking-player (second %)))
       (map (fn [[sq piece]] (check-checker-piece board sq piece)))
       (apply some-fn')))

(defn king-square
  "Returns the square on which is the given color's king."
  [board color]
  (->> (board/piece-placements board)
       (filter (fn [[sq p]]
                 (= [:king color] (pieces/piece-info p))))
       (ffirst)))

(defn moves
  "Returns a list of all the legal moves from this position, ignoring
  the positions' half-move attribute."
  [{:keys [board turn castling en-passant] :as pos}]
  ;; the checker is a pre-optimized (for this position) function for
  ;; determining if a move puts the moving player in check.
  (let [checker (check-checker board (other-color turn))
        king-sq (king-square board turn)]
    (->> (concat (normal-moves board turn)
                 (castling-moves board turn (castling turn))
                 (en-passant-moves board turn en-passant))
         (remove (fn [move]
                   (let [from-sq (moves/primary-from move)
                         to-sq (moves/primary-to move)
                         board' (make-move-board board move)
                         king-sq' (if (= from-sq king-sq) to-sq king-sq)]
                     (checker board' king-sq')))))))

;; Do we hate that the arg format here is not the standard
;; representation of a move?
(defn legal-move?
  [pos from-to-pair]
  (boolean (some (fn [move]
                   (= from-to-pair [(moves/primary-from move)
                                    (moves/primary-to move)]))
                 (moves pos))))

(defn player-to-move-in-check?
  [{:keys [turn board]}]
  (attacks? board (other-color turn) (king-square board turn)))

(defn position-status
  "Returns one of #{:checkmate :stalemate :ongoing}."
  [pos]
  (if (empty? (moves pos))
    (if (player-to-move-in-check? pos)
      :checkmate
      :stalemate)
    :ongoing))

;;
;; Backwards moves!
;;

(def not-kings
  {:white [:P :N :B :R :Q]
   :black [:p :n :b :r :q]})

(defn ray-unmoves
  [directions board sq unmoving-color]
  (for [dir directions
        blank-sq (take-while #(= :_ (board/get board %)) (sqs-in-dir sq dir))
        captured-piece (cons :_ (not-kings (other-color unmoving-color)))]
    (if (= :_ captured-piece)
      (moves/->BasicMove blank-sq sq)
      (moves/->BasicCaptureMove blank-sq sq captured-piece))))

(defn king-and-knight-unmoves
  [dirs board sq unmoving-color]
  (for [blank-sq (king-and-knight-squares dirs sq)
        :let [entry (board/get board blank-sq)]
        :when (= :_ entry)
        captured-piece (cons :_ (not-kings (other-color unmoving-color)))]
    (if (= :_ captured-piece)
      (moves/->BasicMove blank-sq sq)
      (moves/->BasicCaptureMove blank-sq sq captured-piece))))

(def normal-king-unmoves
  (partial king-and-knight-unmoves all-standard-movements))
(def normal-queen-unmoves
  (partial ray-unmoves all-standard-movements))
(def normal-rook-unmoves
  (partial ray-unmoves rectilinear-movements))
(def normal-bishop-unmoves
  (partial ray-unmoves diagonal-movements))
(def normal-knight-unmoves
  (partial king-and-knight-unmoves knight-moves))

(defn normal-pawn-unmoves
  [board sq unmoving-color]
  (let [dir (- (pawn-direction unmoving-color))
        backward (sq/translate-row sq dir)
        jump (sq/translate-row backward dir)
        attack-left (sq/translate sq dir -1)
        attack-right (sq/translate sq dir 1)
        opponent (other-color unmoving-color)]
    (remove nil?
            (apply concat
             [(if (= :_ (board/get board backward))
                [(moves/->PawnForwardMove backward sq)])
              (if (and jump
                       (= (sq/row jump) (pawn-start-row unmoving-color))
                       (= :_ (board/get board backward))
                       (= :_ (board/get board jump)))
                [(moves/->PawnForwardMove jump sq)])
              (if-let [p (and attack-left (board/get board attack-left))]
                (when (= :_ (board/get board attack-left))
                  (for [captured-piece (not-kings opponent)]
                    (moves/->PawnCaptureMove attack-left sq p))))
              (if-let [p (and attack-right (board/get board attack-right))]
                (when (= :_ (board/get board attack-right))
                  (for [captured-piece (not-kings opponent)]
                    (moves/->PawnCaptureMove attack-right sq p))))]))))

(defn normal-unmoves-for-piece
  [board piece color sq]
  ((case (pieces/piece-type piece)
     :king normal-king-unmoves
     :queen normal-queen-unmoves
     :rook normal-rook-unmoves
     :bishop normal-bishop-unmoves
     :knight normal-knight-unmoves
     :pawn normal-pawn-unmoves)
   board
   sq
   color))

(defn normal-unmoves
  [board color-to-unmove]
  (for [[sq p] (board/piece-placements board)
        :when (pieces/color? color-to-unmove p)
        mv (normal-moves-for-piece board p color-to-unmove sq)]
    mv))

(defn castling-unmoves
  [board unmoving-color])

(defn en-passant-unmoves
  [board unmoving-color])

;;
;; TODO:
;;
;; Unpromotions
;;
(defn unmoves
  "Returns all legal backwards moves."
  [{:keys [board turn] :as pos}]
  (let [turn' (other-color turn)
        king-square (king-square board turn)]
    (->> (concat (normal-unmoves board turn')
                 (castling-unmoves board turn')
                 (en-passant-unmoves board turn'))
         (remove (fn [move]
                   (let [board' (moves/apply-backward move board)]
                     (attacks? board' turn' king-square)))))))
