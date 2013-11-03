(ns com.gfredericks.chess.rules-test
  (:require [clojure.set :as sets]
            [clojure.test :refer :all]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.position :as position]
            [com.gfredericks.chess.moves :as moves]
            [com.gfredericks.chess.rules :refer :all]
            [com.gfredericks.chess.squares :refer :all]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [simple-check.clojure-test :refer [defspec]]))

(deftest starting-position-test
  (is (= (set (moves position/initial))
         #{[a2 a3] [b2 b3] [c2 c3] [d2 d3]
           [e2 e3] [f2 f3] [g2 g3] [h2 h3]
           [a2 a4] [b2 b4] [c2 c4] [d2 d4]
           [e2 e4] [f2 f4] [g2 g4] [h2 h4]
           [b1 a3] [b1 c3] [g1 f3] [g1 h3]})))

(deftest legal-moves-test
  (let [pos #chess/fen "r2qk2r/pp3ppp/1npbpnb1/8/3P3N/2N1P1P1/PP2BP1P/R1BQ1RK1 b - - 0 1"
        mvs (set (moves pos))]
    (are [from to] (mvs [from to])
         g6 b1
         h7 h5
         d8 b8
         #_e8 #_g8)
    (are [from to] (not (mvs [from to]))
         g3 g4
         d1 e1
         d4 c5
         e6 f5
         e8 c8
         b7 b5))

  ;; large random position, verification list generated by hand.
  ;; white is in check here
  ;;
  ;;   ---------------------------------
  ;; 8 | N |   |   |   | B |   |   | N |
  ;;   +---+---+---+---+---+---+---+---+
  ;; 7 | k |   | b |   | K |   | q | p |
  ;;   +---+---+---+---+---+---+---+---+
  ;; 6 | q |   | n |   |   |   |   |   |
  ;;   +---+---+---+---+---+---+---+---+
  ;; 5 | b |   |   |   |   | r |   |   |
  ;;   +---+---+---+---+---+---+---+---+
  ;; 4 |   |   |   | P |   | P | p |   |
  ;;   +---+---+---+---+---+---+---+---+
  ;; 3 |   |   | Q | q | P |   | p |   |
  ;;   +---+---+---+---+---+---+---+---+
  ;; 2 |   |   |   |   |   | P |   | B |
  ;;   +---+---+---+---+---+---+---+---+
  ;; 1 |   |   |   |   |   |   | B | r |
  ;;   ---------------------------------
  ;;     a   b   c   d   e   f   g   h
  (let [pos #chess/fen "N3B2N/k1b1K1qp/q1n5/b4r2/3P1Pp1/2QqP1p1/5P1B/6Br w - - 0 1"]
    (is (= (moves pos)
           [[e7 e6]]))
    (is (= (-> pos (make-move [e7 e6]) (moves) (set))
           ;; grouped by the piece doing the moving
           #{[a7 a8] [a7 b8] [a7 b7]

             [c7 b8] [c7 d8] [c7 b6] [c7 d6] [c7 e5] [c7 f4]

             [g7 f8] [g7 g8] [g7 h8] [g7 d7] [g7 e7] [g7 f7]
             [g7 f6] [g7 g6] [g7 h6] [g7 e5] [g7 g5] [g7 d4]

             [h7 h6] [h7 h5]

             [a6 c8] [a6 b7] [a6 b6] [a6 b5] [a6 c4]

             [c6 b8] [c6 d8] [c6 e7] [c6 e5] [c6 b4] [c6 d4]

             [a5 b6] [a5 b4] [a5 c3]

             [f5 f8] [f5 f7] [f5 f6] [f5 b5] [f5 c5] [f5 d5]
             [f5 e5] [f5 g5] [f5 h5] [f5 f4]

             [d3 b5] [d3 c4] [d3 d4] [d3 e4] [d3 c3] [d3 e3]
             [d3 c2] [d3 d2] [d3 e2] [d3 b1] [d3 d1] [d3 f1]

             [g3 f2] [g3 g2] [g3 h2]

             [h1 g1] [h1 h2]}))))


(def castling-pos
  #chess/fen "3k4/8/8/8/8/8/8/4K2R w K - 0 1")

(defn make-moves [pos & moves]
  (reduce make-move pos moves))

(deftest castling-test
  (testing "You can castle in this position"
    (is (legal-move? castling-pos [e1 g1])))
  (testing "You can't castle if the position precludes it"
    (is (not (-> castling-pos
                 (assoc-in [:castling :white :king] false)
                 (legal-move? [e1 g1])))))
  (testing "You can't castle if the rook moves"
    (let [current-moves (set (moves castling-pos))
          new-pos (make-moves castling-pos
                              [h1 h2]
                              [d8 d7]
                              [h2 h1]
                              [d7 d8])
          new-moves (set (moves new-pos))]
      (is (sets/subset? new-moves current-moves))
      (is (= #{[e1 g1]} (sets/difference current-moves new-moves)))))
  (testing "You can't castle if the king is in check at any point"
    (are [sq piece] (-> (update-in castling-pos [:board] board/set sq piece)
                        (legal-move? [e1 g1])
                        (false?))
         ;; direct checks
         e6 :r, c3 :b, d3 :n, a5 :q
         ;; through checks
         f6 :r, d3 :b, e3 :n, h2 :n
         ;; into checks
         g6 :r, e3 :b, g8 :q, h2 :p)))

(def en-passant-pos
  #chess/fen "3k4/8/8/8/2p2p1p/8/1P4P1/4K3 w K - 0 1")

(deftest en-passant-test
  (testing "You can do en passant after a jump"
    (let [pos (make-move en-passant-pos [b2 b4])
          ep-move [c4 b3]]
      (is (legal-move? pos ep-move))
      (let [pos' (make-move pos ep-move)]
        (is (-> pos' :board (board/get b4) (= :_))))))
  (testing "You can't do en passant two moves later"
    (let [pos (make-moves en-passant-pos
                          [b2 b4]
                          [d8 e8]
                          [e1 e2])]
      (is (not (legal-move? pos [c4 b3])))))
  (testing "You can do an en passant from either side"
    (let [pos (make-move en-passant-pos [g2 g4])]
      (is (legal-move? pos [h4 g3]))
      (is (legal-move? pos [f4 g3]))))
  (testing "You can't do an en passant if it wasn't a jump"
    (let [pos (make-moves en-passant-pos
                          [b2 b3]
                          [h4 h3]
                          [b3 b4])]
      (is (not (legal-move? pos [c4 b3])))))
  (testing "En-passant works at the edge of the board"
    (let [pos #chess/fen "3k4/8/8/8/1p6/8/P7/4K3 w K - 0 1"
          move-1 [a2 a4]
          move-2 [b4 a3]]
      (is (legal-move? pos move-1))
      (let [pos' (make-move pos move-1)]
        (is (legal-move? pos' move-2))
        (is (= :P (board/get (:board pos') a4)))
        (let [pos'' (make-move pos move-2)]
          (is (= :_ (board/get (:board pos'') a4))))))))

(defn rand-nth'
  [^java.util.Random r coll]
  (nth coll
       (.nextInt r (count coll))))

(defspec play-a-random-game 10
  ;; Haxy way to do this: generate a thousand random numbers
  ;; and use those for seeds to select moves
  (prop/for-all [seeds (apply gen/tuple
                              (repeat 1000 (gen/choose 0 1000000)))]
    (loop [pos position/initial
           [seed & more] seeds]
      (let [moves (cond->> (moves pos)
                           (= 50 (:half-move pos))
                           (filter moves/progressive?))]
        (if (empty? moves)
          :cool
          (let [move (rand-nth' (java.util.Random. seed) moves)]
            (recur (make-move pos move) more)))))))


;; Moar tests:
;; - test various kinds of check, and how your move choices in
;;   a complex position become vastly fewer
;; - test promotions
;; - test castling where one rook is captured in place and another
;;   moves into his spot.
