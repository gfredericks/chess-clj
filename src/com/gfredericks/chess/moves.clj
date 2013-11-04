(ns com.gfredericks.chess.moves
  (:require [com.gfredericks.chess.board :as b]
            [com.gfredericks.chess.pieces :as pieces]
            [com.gfredericks.chess.squares :as sq]))

(defprotocol IMove
  (apply-forward [move board])
  (apply-backward [move board])
  (progressive? [move]
    "Returns true if the move is a capture or a pawn move.")
  (primary-from [move]
    "Returns the square the piece being moved is moving from.")
  (primary-to [move]
    "Returns the square the piece being moved is moving to."))

(defn ^:private move-piece
  [board from-sq to-sq]
  (-> board
      (b/set from-sq :_)
      (b/set to-sq (b/get board from-sq))))


;; TODO: I'm thinking maybe pawn moves should be their own type...
(defrecord BasicMove [from-sq to-sq]
  IMove
  (apply-forward [_ board]
    (move-piece board from-sq to-sq))
  (apply-backward [_ board]
    (move-piece board to-sq from-sq))
  (progressive? [_] false)
  (primary-from [_] from-sq)
  (primary-to [_] to-sq))

(defrecord BasicCaptureMove [from-sq to-sq captured-piece]
  IMove
  (apply-forward [_ board]
    (move-piece board from-sq to-sq))
  (apply-backward [_ board]
    (-> board
        (move-piece to-sq from-sq)
        (b/set to-sq captured-piece)))
  (progressive? [_] true)
  (primary-from [_] from-sq)
  (primary-to [_] to-sq))

(defrecord PawnForwardMove [from-sq to-sq]
  IMove
  (apply-forward [_ board]
    (move-piece board from-sq to-sq))
  (apply-backward [_ board]
    (move-piece board to-sq from-sq))
  (progressive? [_] true)
  (primary-from [_] from-sq)
  (primary-to [_] to-sq))

(defrecord PawnCaptureMove [from-sq to-sq captured-piece]
  IMove
  (apply-forward [_ board]
    (move-piece board from-sq to-sq))
  (apply-backward [_ board]
    (-> board
        (move-piece to-sq from-sq)
        (b/set to-sq captured-piece)))
  (progressive? [_] true)
  (primary-from [_] from-sq)
  (primary-to [_] to-sq))

;; Some helpers for CastlingMove
(defn ^:private king-from
  [rook-from-sq]
  (sq/set-col rook-from-sq 4))
(defn ^:private king-target
  [rook-from-sq]
  (sq/set-col rook-from-sq
              (if (zero? (sq/col rook-from-sq)) 2 6)))
(defn ^:private rook-target
  [rook-from-sq]
  (sq/set-col rook-from-sq
              (if (zero? (sq/col rook-from-sq)) 3 5)))
(defrecord CastlingMove [rook-from-sq]
  IMove
  (apply-forward [_ board]
    (-> board
        (move-piece rook-from-sq
                    (rook-target rook-from-sq))
        (move-piece (king-from rook-from-sq)
                    (king-target rook-from-sq))))
  (apply-backward [_ board]
    (-> board
        (move-piece (rook-target rook-from-sq)
                    rook-from-sq)
        (move-piece (king-target rook-from-sq)
                    (king-from rook-from-sq))))
  (progressive? [_] false)
  (primary-from [_]
    (king-from rook-from-sq))
  (primary-to [_]
    (king-target rook-from-sq)))

;; the capture-square and captured-piece fields can be inferred from
;; the from-sq and to-sq fields, but doing it this way allows us to
;; not actually deal with pieces directly
(defrecord EnPassantMove [from-sq to-sq capture-square captured-piece]
  IMove
  (apply-forward [_ board]
    (-> board
        (move-piece from-sq to-sq)
        (b/set capture-square :_)))
  (apply-backward [_ board]
    (-> board
        (move-piece to-sq from-sq)
        (b/set capture-square captured-piece)))
  (progressive? [_] true)
  (primary-from [_] from-sq)
  (primary-to [_] to-sq))

(defrecord PromotionMove [from-sq to-sq pawn promoted-to]
  IMove
  (apply-forward [_ board]
    (-> board
        (b/set from-sq :_)
        (b/set to-sq promoted-to)))
  (apply-backward [_ board]
    (-> board
        (b/set from-sq pawn)
        (b/set to-sq :_)))
  (progressive? [_] true)
  (primary-from [_] from-sq)
  (primary-to [_] to-sq))

(defrecord PromotionCapture [from-sq to-sq pawn promoted-to captured-piece]
  IMove
  (apply-forward [_ board]
    (-> board
        (b/set from-sq :_)
        (b/set to-sq promoted-to)))
  (apply-backward [_ board]
    (-> board
        (b/set from-sq pawn)
        (b/set to-sq captured-piece)))
  (progressive? [_] true)
  (primary-from [_] from-sq)
  (primary-to [_] to-sq))

(defn en-passant-square
  "If the move is a pawn jump, returns the in-between square.
  Else returns nil."
  [move]
  (if (instance? PawnForwardMove move)
    (let [from-sq (.from_sq ^PawnForwardMove move)
          to-sq (.to_sq ^PawnForwardMove move)
          from-row (sq/row from-sq)
          to-row (sq/row to-sq)
          d (- from-row to-row)]
      (if (or (= 2 d) (= -2 d))
        (sq/square (sq/col from-sq)
                   (bit-shift-right (+ from-row to-row) 1))))))

(def ^:private ^:const no-castling {:king false :queen false})

(defn update-castling
  "Given a map of the form:

    {:white {:king true, :queen false}
     :black {:king true, :queen true}}

  And a move, returns a new map with maybe some of the entries
  set to false as appropriate."
  [m move]
  ;; smelly!
  (cond

   (instance? CastlingMove move)
   (assoc m (-> move :rook-from-sq sq/row ({0 :white 7 :black}))
          no-castling)

   (or (instance? BasicMove move)
       (instance? BasicCaptureMove move))
   (condp = ((juxt sq/row sq/col) (:from-sq move))
     [0 4] (assoc m :white no-castling)
     [0 0] (assoc-in m [:white :queen] false)
     [0 7] (assoc-in m [:white :king] false)
     [7 4] (assoc m :black no-castling)
     [7 0] (assoc-in m [:black :queen] false)
     [7 7] (assoc-in m [:black :king] false)
     m)

   :else m))
