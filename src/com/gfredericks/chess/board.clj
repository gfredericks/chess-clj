(ns com.gfredericks.chess.board
  (:refer-clojure :exclude [get set])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [com.gfredericks.chess.squares :as sq]))

(s/def ::board any?)

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

(def pieces' (into {} (map (comp vec reverse) pieces)))

(defn ^:private outer-index
  [^long sq]
  (bit-shift-right sq 4))
(defn ^:private inner-index
  [^long sq]
  (-> sq
       (bit-and 15)
       (bit-shift-left 2)))

(defn get
  [b ^long sq]
  (let [x (core/get b (outer-index sq))]
    (-> x
        (bit-shift-right (inner-index sq))
        (bit-and 15)
        (pieces'))))

(defn set
  [b ^long sq piece]
  (let [piece-code (pieces piece)
        inner (inner-index sq)
        mask (bit-shift-left 15 inner)]
    (update-in b [(outer-index sq)]
               #(-> %
                    ;; clear the relevant 4 bits
                    (bit-and (bit-not mask))
                    ;; insert the piece-code
                    (bit-or (bit-shift-left piece-code inner))))))

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
                         (recur (conj ret [(keyword (name x)) (sq/square i row-num)])
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
                             :let [entries (map #(get b (sq/square % row-num)) (range 8))]]
                         (loop [s "", entries entries]
                           (if-let [[e & more] (seq entries)]
                             (if (= :_ e)
                               (let [[as bs] (split-with #{:_} more)]
                                 (recur (str s (inc (count as))) bs))
                               (recur (str s (name e)) more))
                             s)))))

(defn print-board-unicode
  [board]
  (print "  ┏")
  (dotimes [_ 7]
    (print "━━━┯"))
  (println "━━━┓")
  (doseq [row (range 7 -1 -1)]
    (print (inc row) \┃)
    (doseq [col (range 8)
            :let [piece (get board (sq/square col row))
                  piece-str (if (= :_ piece) \space
                                (case piece
                                  :K "♔" :Q "♕" :R "♖"
                                  :B "♗" :N "♘" :P "♙"
                                  :k "♚" :q "♛" :r "♜"
                                  :b "♝" :n "♞" :p "♟︎"))]]
      (print (str \space piece-str \space (if (= 7 col) \┃ \│))))
    (print \newline)
    (when (pos? row)
      (print "  ┠")
      (dotimes [_ 7]
        (print "───┼"))
      (println "───┨")))
  (print "  ┗")
  (dotimes [_ 7]
    (print "━━━┷"))
  (println "━━━┛")
  (println "   " (apply str (for [x "abcdefgh"] (str x "   ")))))

(defn print-board-ascii
  [board]
  (println " " (apply str (repeat 33 \-)))
  (doseq [row (range 7 -1 -1)]
    (print (inc row) \|)
    (doseq [col (range 8)
            :let [piece (get board (sq/square col row))
                  piece-str (if (= :_ piece) \space (name piece))]]
      (print (str \space piece-str \space \|)))
    (print \newline)
    (when (pos? row)
      (print (apply str "  " (repeat 8 "+---")))
      (println \+)))
  (println (apply str "  " (repeat 33 \-)))
  (println "   " (apply str (for [x "abcdefgh"] (str x "   ")))))

(def print-board print-board-unicode)

(defn piece-placements
  "Returns a sequence of tuples: [sq piece]."
  [board]
  (loop [ret (transient [])
         outer-index 0
         inner-index 0
         outer-i 1
         ^long x (core/get board 0)]
    (if (= 16 inner-index)
      (if (= 4 outer-i)
        (persistent! ret)
        (recur ret (+ outer-index 16) 0 (inc outer-i) (core/get board outer-i)))
      (let [p (bit-and x 15)
            ret' (if (zero? p) ret (conj! ret [(+ outer-index inner-index) (pieces' p)]))]
        (recur ret' outer-index (inc inner-index) outer-i (bit-shift-right x 4))))))
