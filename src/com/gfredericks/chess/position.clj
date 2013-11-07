(ns com.gfredericks.chess.position
  (:require [clojure.string :as s]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.squares :as sq]))

(defn- vecs
  [coll]
  (vec (map vec coll)))


(defn algebraic-square->pair
  [alg-square]
  (->> alg-square seq ((fn [[a b]] (sq/square ({\a 0 \b 1 \c 2 \d 3
                                                \e 4 \f 5 \g 6 \h 7}
                                               a)
                                              (dec (read-string (str b))))))))

(defn pair->algebraic-square
  "Returns a string"
  [sq]
  (let [row (sq/row sq)
        col (sq/col sq)]
    (str (nth "abcdefgh" col)
         (inc row))))

(defn read-fen
  [fen]
  (let [[board turn castling ep half full] (re-seq #"\S+" fen),
        castling (set castling)]
    (with-meta
      {:board (board/read-fen-board board)
       :turn ({"w" :white "b" :black} turn)
       :castling {:white {:king (contains? castling \K),
                          :queen (contains? castling \Q)}
                  :black {:king (contains? castling \k)
                          :queen (contains? castling \q)}}
       :en-passant (when-not (= "-" ep)
                     (algebraic-square->pair ep))
       :half-move (read-string half)
       :full-move (read-string full)}
      {:type ::position})))

(defn fen-castling
  "Returns the castling portion of a fen string."
  [castling]
  (let [s (->> [[[:white :king] \K]
                [[:white :queen] \Q]
                [[:black :king] \k]
                [[:black :queen] \q]]
               (keep #(if (get-in castling (first %)) (second %)))
               (apply str))]
    (if (empty? s) "-" s)))

(defn ->fen
  [{:keys [board turn castling en-passant half-move full-move]}]
  (clojure.string/join " "
                       [(board/board->fen-board board)
                        ({:white "w", :black "b"} turn)
                        (fen-castling castling)
                        (if en-passant
                          (pair->algebraic-square en-passant)
                          "-")
                        half-move
                        full-move]))

(defmethod print-method ::position
  [pos ^java.io.Writer w]
  (.write w "#chess/fen ")
  (print-method (->fen pos) w))

(def initial (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

(defn print-position
  [{:keys [board turn castling en-passant half-move full-move]}]
  (println (apply str (repeat 40 "=")))
  (board/print-board board)
  (printf "%d: %s to move\n" full-move (name turn))
  (printf "(Castling: %s%s)\n"
          (fen-castling castling)
          (if en-passant (str ", en passant: " en-passant) ""))
  (println "half-move:" half-move)
  (println (apply str (repeat 40 "="))))
