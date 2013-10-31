(ns com.gfredericks.chess.pieces)

(def piece-color
  {:K :white
   :Q :white
   :R :white
   :B :white
   :N :white
   :P :white
   :k :black
   :q :black
   :r :black
   :b :black
   :n :black
   :p :black})

(def piece-type
  {:K :king
   :Q :queen
   :R :rook
   :B :bishop
   :N :knight
   :P :pawn
   :k :king
   :q :queen
   :r :rook
   :b :bishop
   :n :knight
   :p :pawn})

(def piece-info (juxt piece-type piece-color))
