(ns chess.bench
  (:use chess.core))

(def midgame-pos
  (FEN->pos "r2qk2r/pp3ppp/1npbpnb1/8/3P3N/2N1P1P1/PP2BP1P/R1BQ1RK1 b kq - 2 11"))

(defn go
  []
  (let [f #(->> %
                ((juxt moves unmoves))
                (apply concat)
                count)]
    (dotimes [n 5] (f starting-pos)) ;; warmup
    (println "Initial")
    (time (dotimes [n 10] (f starting-pos)))
    (println "Midgame")
    (time (dotimes [n 10] (f midgame-pos)))))