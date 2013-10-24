(ns com.gfredericks.chess.rules
  "Which moves are legal and such.

  A move is just a [from-square to-square]."
  (:require [com.gfredericks.chess.position :refer [get-piece set-piece]]))

(def squares
  (for [col (range 8)
        row (range 8)]
    [col row]))

(defn rays
  [])

(defn move-candidates-for-piece
  "Returns all the squares "
  [pos sq])

(defn moves
  "Returns a list of all the legal moves from this position, ignoring
  the positions' half-move and full-move."
  [{:keys [board turn castling en-passant]}]
  )
