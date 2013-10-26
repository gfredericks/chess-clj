(ns com.gfredericks.chess.rules-test
  (:require [clojure.set :as sets]
            [clojure.test :refer :all]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.rules :refer :all]))

(defn kw->sq
  [kw]
  (->> kw name seq ((fn [[a b]] [(dec (read-string (str b)))
                                 ({\a 0 \b 1 \c 2 \d 3
                                   \e 4 \f 5 \g 6 \h 7}
                                  a)]))))

(deftest legal-moves-test
  (let [pos #chess/fen "r2qk2r/pp3ppp/1npbpnb1/8/3P3N/2N1P1P1/PP2BP1P/R1BQ1RK1 b - - 0 1"
        mvs (set (moves pos))]
    (are [from to] (mvs [(kw->sq from) (kw->sq to)])
         :g6 :b1
         :h7 :h5
         :d8 :b8
         #_[:e 8] #_[:g 8])
    (are [from to] (not (mvs [from to]))
         :g3 :g4
         :d1 :e1
         :d4 :c5
         :e6 :f5
         :e8 :c8
         :b7 :b5)))

(def castling-pos
  #chess/fen "3k4/8/8/8/8/8/8/4K2R w K - 0 1")

(defn kws->move [from to] [(kw->sq from) (kw->sq to)])

(defn make-moves [pos & moves]
  (reduce make-move pos (map (fn [[k1 k2]] (kws->move k1 k2)) moves)))

(deftest castling-test
  (testing "You can castle in this position"
    (is (legal-move? castling-pos (kws->move :e1 :g1))))
  (testing "You can't castle if the position precludes it"
    (is (not (-> castling-pos
                 (assoc-in [:castling :white :king] false)
                 (legal-move? (kws->move :e1 :g1))))))
  (testing "You can't castle if the rook moves"
    (let [current-moves (set (moves castling-pos))
          new-pos (make-moves castling-pos
                              [:h1 :h2]
                              [:d8 :d7]
                              [:h2 :h1]
                              [:d7 :d8])
          new-moves (set (moves new-pos))]
      (is (sets/subset? new-moves current-moves))
      (is (= #{[[0 4] [0 6]]} (sets/difference current-moves new-moves)))))
  (testing "You can't castle if the king is in check at any point"
    (are [sq piece] (-> (update-in castling-pos [:board] board/set sq piece)
                        (legal-move? [[0 4] [0 6]])
                        (false?))
         ;; direct checks
         [5 4] :r, [2 2] :b, [2 3] :n, [4 0] :q
         ;; through checks
         [5 5] :r, [2 3] :b, [2 4] :n, [1 7] :n
         ;; into checks
         [5 6] :r, [2 4] :b, [7 6] :q, [1 7] :p)))

(def en-passant-pos
  #chess/fen "3k4/8/8/8/2p2p1p/8/1P4P1/4K3 w K - 0 1")

(deftest en-passant-test
  (testing "You can do en passant after a jump"
    (let [pos (make-move en-passant-pos [[1 1] [3 1]])
          ep-move [[3 2] [2 1]]]
      (is (legal-move? pos ep-move))
      (let [pos' (make-move pos ep-move)]
        (is (-> pos' :board (board/get [3 1]) (= :_))))))
  (testing "You can't do en passant two moves later"
    (let [pos (make-moves en-passant-pos
                          [:b2 :b4]
                          [:d8 :e8]
                          [:e1 :e2])]
      (is (not (legal-move? pos [[3 2] [2 1]])))))
  (testing "You can do an en passant from either side"
    (let [pos (make-move en-passant-pos [[1 6] [3 6]])]
      (is (legal-move? pos [[3 7] [2 6]]))
      (is (legal-move? pos [[3 5] [2 6]]))))
  (testing "You can't do an en passant if it wasn't a jump"
    (let [pos (make-moves en-passant-pos
                          [:b2 :b3]
                          [:h4 :h3]
                          [:b3 :b4])]
      (is (not (legal-move? pos [[3 2] [2 1]]))))))


;; Moar tests:
;; - test various kinds of check, and how your move choices in
;;   a complex position become vastly fewer
;; - test promotions
