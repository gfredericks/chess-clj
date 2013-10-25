(ns com.gfredericks.chess.board
  (:refer-clojure :exclude [get set])
  (:require [clojure.core :as core]))

(def all-squares
  (for [row (range 8), col (range 8)] [row col]))

(declare board->fen-board)

(def empty-board (with-meta [0 0 0 0] {:type ::board}))

(defmethod print-method ::board
  [b ^java.io.Writer pw]
  (.write pw "#chess/fen-board ")
  (print-method (board->fen-board b) pw))

(def pieces
  {:_ 0
   :K 1
   :Q 2
   :R 3
   :B 4
   :N 5
   :P 6
   :k 7
   :q 8
   :r 9
   :b 10
   :n 11
   :p 12})

(def piece-color
  {:K :white
   :Q :white
   :R :white
   :B :white
   :N :white
   :P :white
   :k :black
   :q :black
   :r :black
   :b :black
   :n :black
   :p :black})

(def piece-type
  {:K :king
   :Q :queen
   :R :rook
   :B :bishop
   :N :knight
   :P :pawn
   :k :king
   :q :queen
   :r :rook
   :b :bishop
   :n :knight
   :p :pawn})

(def piece-info (juxt piece-type piece-color))

(def pieces' (into {} (map (comp vec reverse) pieces)))

(defn indices
  [row col]
  [(bit-shift-right row 1)
   (-> row
       (bit-and 1)
       (bit-shift-left 3)
       (bit-or col)
       (bit-shift-left 2))])

(defn get
  [b [row col]]
  (let [[outer inner] (indices row col)
        x (core/get b outer)]
    (-> x
        (bit-shift-right inner)
        (bit-and 15)
        (pieces'))))

(defn set
  [b [row col] piece]
  (let [piece-code (pieces piece)
        [outer inner] (indices row col)
        mask (bit-shift-left 15 inner)]
    (update-in b [outer]
               #(-> %
                    ;; clear the relevant 4 bits
                    (bit-and (bit-not mask))
                    ;; insert the piece-code
                    (bit-or (bit-shift-left piece-code inner))))))

(def color-at (comp piece-color get))

(defn ^:private fen-board->piece-locations
  "Returns a sequence of [piece square] tuples."
  [fen-board]
  (->> (clojure.string/split fen-board #"\/")
       (map list (range 7 -1 -1))
       (mapcat (fn [[row-num fen-row]]
                 (loop [ret (), i 0, s fen-row]
                   (if-let [[c & more] (seq s)]
                     (let [x (read-string (str c))]
                       (if (number? x)
                         (recur ret (+ i x) more)
                         (recur (conj ret [(keyword (name x)) [row-num i]])
                                (inc i)
                                more)))
                     ret))))))

(defn read-fen-board
  "Given the board portion of a FEN string, returns the board it
  represents."
  [s]
  (reduce (fn [b [p sq]]
            (set b sq p))
          empty-board
          (fen-board->piece-locations s)))

(defn board->fen-board
  "Given a board, returns the board portion of the FEN string."
  [b]
  (clojure.string/join "/"
                       (for [row-num (range 7 -1 -1)
                             :let [entries (map #(get b [row-num %]) (range 8))]]
                         (loop [s "", entries entries]
                           (if-let [[e & more] (seq entries)]
                             (if (= :_ e)
                               (let [[as bs] (split-with #{:_} more)]
                                 (recur (str s (inc (count as))) bs))
                               (recur (str s (name e)) more))
                             s)))))

(defn print-board
  [board]
  (println (apply str (repeat 33 \_)))
  (doseq [row (range 7 -1 -1)]
    (print \|)
    (doseq [col (range 8)
            :let [piece (get board [row col])
                  piece-str (if (= :_ piece) \space (name piece))]]
      (print (str \space piece-str \space \|)))
    (print \newline)
    (when (pos? row)
      (print (apply str (repeat 8 "+---")))
      (println \+)))
  (println (apply str (repeat 33 \-))))
