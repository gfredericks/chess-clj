(ns com.gfredericks.chess.position
  (:require [clojure.string :as s]
            [com.gfredericks.chess.board :as board]))

(defn- vecs
  [coll]
  (vec (map vec coll)))

(defn FEN->pos
  [fen]
  (let [[board turn castling ep half full] (re-seq #"\S+" fen),
        castling (set castling)
        ep (seq ep)]
    {:board (board/read-fen-board board)
     :turn ({"w" :white "b" :black} turn)
     :castling {:white {:king (contains? castling \K),
                        :queen (contains? castling \Q)}
                :black {:king (contains? castling \k)
                        :queen (contains? castling \q)}}
     :en-passant (when-not (= [\-] ep)
                   (let [[a b] ep]
                     [(keyword (str a)) (read-string (str b))]))
     :half-move (read-string half)
     :full-move (read-string full)}))

(def starting-pos (FEN->pos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

(defn get-piece
  [pos sq]
  (-> pos :board (board/get sq)))

(defn set-piece
  [pos sq p]
  (update-in pos [:board] board/set sq p))
