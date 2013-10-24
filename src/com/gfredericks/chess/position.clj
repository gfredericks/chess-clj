(ns com.gfredericks.chess.position)

(defn- vecs
  [coll]
  (vec (map vec coll)))

(defn FEN->pos
  [fen]
  (let [[board turn castling ep half full] (re-seq #"\S+" fen),
        board (for [rank (s/split board #"\/")]
                (mapcat (fn [c] (if (#{\1 \2 \3 \4
                                       \5 \6 \7 \8} c)
                                  (-> c str read-string (repeat :_))
                                  (-> c str keyword list)))
                        rank))
        castling (set castling)
        ep (seq ep)]
    {:board (vecs (reverse board))
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
  [pos [col row]]
  (get-in pos [:board row col]))

(defn set-piece
  [pos [col row] p]
  (assoc-in pos [:board row col] p))
