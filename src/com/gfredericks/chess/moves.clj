(ns com.gfredericks.chess.moves
  (:require [com.gfredericks.chess.board :as b]
            [com.gfredericks.chess.squares :as sq]))

(defprotocol IMove
  (apply-forward [move board])
  (apply-backward [move board]))

(defn ^:private move-piece
  [board from-sq to-sq]
  (-> board
      (b/set from-sq :_)
      (b/set to-sq (b/get board from-sq))))

(defrecord BasicMove [from-sq to-sq]
  IMove
  (apply-forward [_ board]
    (move-piece board from-sq to-sq))
  (apply-backward [_ board]
    (move-piece board to-sq from-sq)))

(defrecord BasicCaptureMove [from-sq to-sq captured-piece]
  IMove
  (apply-forward [_ board]
    (move-piece board from-sq to-sq))
  (apply-backward [_ board]
    (-> board
        (move-piece to-sq from-sq)
        (b/set to-sq captured-piece))))

(defrecord CastlingMove [rook-from-sq]
  IMove
  (apply-forward [_ board]
    (let [row (sq/row rook-from-sq)
          queenside? (zero? (sq/col rook-from-sq))
          sq #(sq/square % row)
          king-from-sq (sq 4)
          king-target (sq (if queenside? 2 6))
          rook-target (sq (if queenside? 3 5))]
      (-> board
          (move-piece rook-from-sq rook-target)
          (move-piece king-from-sq king-target))))
  (apply-backward [_ board]
    (let [row (sq/row rook-from-sq)
          queenside? (zero? (sq/col rook-from-sq))
          sq #(sq/square % row)
          king-from-sq (sq 4)
          king-target (sq (if queenside? 2 6))
          rook-target (sq (if queenside? 3 5))]
      (-> board
          (move-piece rook-target rook-from-sq)
          (move-piece king-target king-from-sq)))))

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
        (b/set capture-square captured-piece))))

(defrecord PromotionMove [from-sq to-sq pawn promoted-to]
  IMove
  (apply-forward [_ board]
    (-> board
        (b/set from-sq :_)
        (b/set to-sq promoted-to)))
  (apply-backward [_ board]
    (-> board
        (b/set from-sq pawn)
        (b/set to-sq :_))))

(defrecord PromotionCapture [from-sq to-sq pawn promoted-to captured-piece]
  IMove
  (apply-forward [_ board]
    (-> board
        (b/set from-sq :_)
        (b/set to-sq promoted-to)))
  (apply-backward [_ board]
    (-> board
        (b/set from-sq pawn)
        (b/set to-sq captured-piece))))
