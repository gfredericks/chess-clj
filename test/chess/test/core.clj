(ns chess.test.core
  (:use [chess.core])
  (:use [clojure.test]))

#_(deftest normal-games-test
  (doseq [game (parse-pgn (slurp "normal-games.pgn"))]
    (loop [pos init-pos, moves (:moves game)]
      (when-let [[m & more] (seq moves)]
        (let [new-pos (make-move pos m)]
          (is new-pos)
          (recur new-pos more))))))

(defn kw->sq
  [kw]
  (->> kw name seq (map str) ((fn [[a b]] [(keyword a) (read-string b)]))))

(deftest legal-moves-test
  (let [pos (FEN->pos "r2qk2r/pp3ppp/1npbpnb1/8/3P3N/2N1P1P1/PP2BP1P/R1BQ1RK1 b kq - 2 11"),boa
        mvs (->> pos moves (map first) set),
        umvs (->> pos unmoves (map first) set)]
    ;; forward moves
    (are [from to] (mvs {:from from :to to :promotion '_.0})
         [:g 6] [:b 1]
         [:h 7] [:h 5]
         [:d 8] [:b 8]
         #_[:e 8] #_[:g 8])
    (are [from to] (not (mvs {:from from :to to :promotion '_.0}))
         [:g 3] [:g 4]
         [:d 1] [:e 1]
         [:d 4] [:c 5]
         [:e 6] [:f 5]
         [:e 8] [:c 8]
         [:b 7] [:b 5])
    ;; backward moves
    (are [from to] (umvs {:from (kw->sq from) :to (kw->sq to) :promotion '_.0})
         :c4 :e2
         :d2 :d4
         :b1 :a1
         :g2 :g1
         :g2 :g3
         :a4 :d1
         :h5 :e2
         :f5 :h4
         #_:e1 #_:g1)

    (are [from to] (not (umvs {:from (kw->sq from) :to (kw->sq to) :promotion '_.0}))
         :f8 :d6
         :c7 :c6
         :a3 :c1
         :b1 :c1
         :f4 :e2
         :h3 :h4
         :g6 :h4
         :a5 :b6)))


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