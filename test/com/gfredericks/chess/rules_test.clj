(ns com.gfredericks.chess.rules-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.chess.rules :refer :all]
            [com.gfredericks.chess.position :refer [FEN->pos]]))

#_(deftest normal-games-test
  (doseq [game (parse-pgn (slurp "normal-games.pgn"))]
    (loop [pos init-pos, moves (:moves game)]
      (when-let [[m & more] (seq moves)]
        (let [new-pos (make-move pos m)]
          (is new-pos)
          (recur new-pos more))))))

(defn kw->sq
  [kw]
  (->> kw name seq ((fn [[a b]] [(dec (read-string (str b)))
                                 ({\a 0 \b 1 \c 2 \d 3
                                   \e 4 \f 5 \g 6 \h 7}
                                  a)]))))

(deftest legal-moves-test
  (let [board #chess/fen-board "r2qk2r/pp3ppp/1npbpnb1/8/3P3N/2N1P1P1/PP2BP1P/R1BQ1RK1"
        mvs (set (normal-moves board :black))]
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


;;
;; find-game tests
;;

(comment
  (deftest find-game-test
    (is (= [] (find-game starting-pos)))
    (is (= [{:from [:e 2]
             :to [:e 4]
             :promotion '_.0}]
           (-> "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
               FEN->pos
               find-game)))))
