(ns com.gfredericks.chess.rules-test
  (:require [clojure.test :refer :all]
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

(deftest castling-test
  (testing "You can castle in this position"
    (is (legal-move? castling-pos (kws->move :e1 :g1)))))
