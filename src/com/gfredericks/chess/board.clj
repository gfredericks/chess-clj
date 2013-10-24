(ns com.gfredericks.chess.board
  (:refer-clojure :exclude [get set])
  (:require [clojure.core :as core]))

(def all-squares
  (for [row (range 8), col (range 8)] [row col]))

(def empty-board [0 0 0 0])

(def pieces
  {:_ 0
   :K 1
   :Q 2
   :B 3
   :N 4
   :P 5
   :k 6
   :q 7
   :b 8
   :n 9
   :p 10})

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
