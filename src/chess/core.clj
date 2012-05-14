(ns chess.core
  (:refer-clojure :exclude [==])
  (:use [clojure.tools.macro :only [macrolet]]
        clojure.core.logic
        chess.logic)
  (:require [clojure.string :as s]))

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
    {:board (vecs board)
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

(defn moves
  [pos]
  (run* [q]
        (fresh [move after-pos]
               (== q [move after-pos])
               (legal-moveo (select-keys pos [:board :turn]) move after-pos))))

(defn unmoves
  [pos]
  (run* [q]
        (fresh [move before-pos]
               (== q [move before-pos])
               (legal-moveo before-pos move (select-keys pos [:board :turn])))))

;;
;; search
;;

(defn- legal-piece-set?
  [pieces]
  (let [{:strs [k q r b n p] :or {k 0 q 0 r 0 b 0 n 0 p 0}}
        (frequencies (map #(-> % name .toLowerCase) pieces)),
        extras (apply + (filter pos? [(dec q)
                                      (- r 2)
                                      (- b 2)
                                      (- n 2)]))]
    (and (= 1 k)
         (<= (+ extras p) 8))))

;; TODO: also check the colors of the bishops
(defn illegal-piece-sets?
  [board]
  (let [pieces (apply concat board)
        whites (filter #{:K :Q :R :B :N :P} pieces)
        blacks (filter #{:k :q :r :b :n :p} pieces)]
    (or (not (legal-piece-set? whites))
        (not (legal-piece-set? blacks)))))

(defn pieces-missing
  "Count the pieces, subtract from 32."
  [board]
  (->> board
       (apply concat)
       (remove #{:_})
       (count)
       (- 32)))

;; Due to castling this function is not strictly correct -- i.e., in pathological
;; cases it can return a higher number than the number of moves from the start.
;; E.g., both sides move king-pawn, king-knight, king-bishop, then castle. Took
;; 8 moves but function would return 10. We could fix it by specifically looking
;; for a king-rook in the just-castled position.
(defn pieces-not-in-position
  [board]
  (let [start-rows (apply concat (concat (take 2 board) (drop 6 board)))]
    (count
     (remove (fn [[a b]] (= a b))
             (map list
                  start-rows
                  (concat [:r :n :b :q :k :b :n :r]
                          (repeat 8 :p)
                          (repeat 8 :P)
                          [:R :N :B :Q :K :B :N :R]))))))

(defn distance
  "Estimator for distance from starting position. Used for A* search.
  Should return a positive integer unless pos IS the starting position.
  Return value represents minimum number of moves from starting position."
  [{board :board}]
  (if (or (illegal-piece-sets? board))
    Long/MAX_VALUE
    (max (pieces-missing board)
         (pieces-not-in-position board))))


;; These two functions I just translated from the wikipedia pseudocode
;; for A* search. Could probably rewrite it more functionally.
(defn reconstruct-path
  [came-from node]
  (if-let [[move pos] (came-from node)]
    (cons move (reconstruct-path came-from pos))
    []))

(defn find-game
  "Given a position, searches for and maybe returns a sequence of moves
  such that making those moves from the starting position returns the
  given position."
  [pos]
  (let [pos (select-keys pos [:board :turn])
        dist (distance pos)]
    (loop [{:keys [closed open came-from
                   g-score h-score f-score] :as state}
           {:closed #{}
            ;; could optimize this with a sorted-set
            :open #{pos}
            :came-from {}
            :g-score {pos 0}
            :h-score {pos dist}
            :f-score {pos dist}}]
      (when-not (empty? open)
        (let [current (first (sort-by f-score open))]
          (if (zero? (h-score current))
            (reconstruct-path came-from current)
            (recur
             (reduce (fn [state [unmove unposition]]
                       (if (closed unposition)
                         state
                         (let [tent-g (inc (g-score current))
                               neighbor-open? (boolean ((:open state) unposition))
                               open' (-> state :open (conj unposition))
                               h-score' (if neighbor-open?
                                          (:h-score state)
                                          (assoc (:h-score state)
                                            unposition
                                            (distance unposition)))
                               state' (assoc state :open open' :h-score h-score')]
                           (if (= (h-score' unposition) Long/MAX_VALUE)
                             state
                             (if (or (not neighbor-open?)
                                     (< tent-g (-> state :g-score (get unposition))))
                               (-> state'
                                   (assoc-in [:came-from unposition] [unmove current])
                                   (assoc-in [:g-score unposition] tent-g)
                                   (assoc-in [:f-score unposition] (+ tent-g (h-score' unposition))))
                               state')))))
                     (-> state
                         (update-in [:open] disj current)
                         (update-in [:closed] conj current))
                     ;; we want to filter out the moves that lead to positions
                     ;; with infinite distance :/
                     (unmoves current)))))))))

(comment
  (do
    (use 'clojure.repl 'clojure.pprint)
    (defmacro run**
      [stuff & body]
      `(run* [q#] (fresh ~stuff (== q# ~stuff) ~@body))))
  (let [pos (FEN->pos "r2qk2r/pp3ppp/1npbpnb1/8/3P3N/2N1P1P1/PP2BP1P/R1BQ1RK1 b kq - 2 11")]
    (println (count (moves pos)))
    (pprint @clojure.core.logic/point-counts))
  (let [blank-board (->> :_ (repeat 8) (repeat 8) vecs)
        easy-board (-> blank-board
                       (assoc-in [2 4] :K)
                       (assoc-in [4 7] :k)
                       (assoc-in [1 1] :P))
        easy-pos {:board easy-board :turn :white}]
    (-> easy-pos moves count println)
    (pprint @clojure.core.logic/point-counts))

  (def blank-pos (FEN->pos "8/8/8/8/8/8/8/8 w - - 0 1"))
  (def two-kings (-> blank-pos (assoc-in [:board 2 2] :K) (assoc-in [:board 5 5] :k)))
  (def e4 (FEN->pos "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")))