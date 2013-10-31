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

(defn color?
  [color piece]
  (= color (piece-color piece)))

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

(defn pawn?
  [p]
  (case p :P true :p true false))

(defn blank?
  [p]
  (= :_ p))

(def piece-info (juxt piece-type piece-color))
