(ns com.gfredericks.chess.generators
  (:require [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.squares :as sq]
            [simple-check.generators :as gen]))

(def square
  (gen/elements sq/all-squares))

(def piece (gen/elements (keys board/pieces)))

(def board
  "Generates a naive board with random entries."
  (gen/fmap
   (fn [pieces]
     (reduce
      (fn [b [piece sq]]
        (board/set b sq piece))
      board/empty-board
      (map list pieces sq/all-squares)))
   (apply gen/tuple (repeat 64 piece))))
