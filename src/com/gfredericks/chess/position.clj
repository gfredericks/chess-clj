(ns com.gfredericks.chess.position
  (:require [clojure.string :as s]
            [com.gfredericks.chess.board :as board]))

(defn- vecs
  [coll]
  (vec (map vec coll)))


(defn algebraic-square->pair
  [alg-square]
  (->> alg-square name seq ((fn [[a b]] [(dec (read-string (str b)))
                                         ({\a 0 \b 1 \c 2 \d 3
                                           \e 4 \f 5 \g 6 \h 7}
                                          a)]))))

(defn pair->algebraic-square
  "Returns a string"
  [[row col]]
  (str (nth "abcdefgh" col)
       (inc row)))

(defn read-fen
  [fen]
  (let [[board turn castling ep half full] (re-seq #"\S+" fen),
        castling (set castling)
        ep (seq ep)]
    (with-meta
      {:board (board/read-fen-board board)
       :turn ({"w" :white "b" :black} turn)
       :castling {:white {:king (contains? castling \K),
                          :queen (contains? castling \Q)}
                  :black {:king (contains? castling \k)
                          :queen (contains? castling \q)}}
       :en-passant (when-not (= [\-] ep)
                     (algebraic-square->pair ep))
       :half-move (read-string half)
       :full-move (read-string full)}
      {:type ::position})))

(defn ->fen
  [{:keys [board turn castling en-passant half-move full-move]}]
  (clojure.string/join " "
                       [(board/board->fen-board board)
                        ({:white "w", :black "b"} turn)
                        (let [s (->> [[[:white :king] \K]
                                      [[:white :queen] \Q]
                                      [[:black :king] \k]
                                      [[:black :queen] \q]]
                                     (keep #(if (get-in castling (first %)) (second %)))
                                     (apply str))]
                          (if (empty? s) "-" s))
                        (if en-passant
                          (pair->algebraic-square en-passant)
                          "-")
                        half-move
                        full-move]))

(defmethod print-method ::position
  [pos ^java.io.Writer w]
  (.write w "#chess/fen ")
  (print-method (->fen pos) w))

(def starting-pos (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

(defn get-piece
  [pos sq]
  (-> pos :board (board/get sq)))

(defn set-piece
  [pos sq p]
  (update-in pos [:board] board/set sq p))
