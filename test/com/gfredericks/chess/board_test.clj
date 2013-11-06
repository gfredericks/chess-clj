(ns com.gfredericks.chess.board-test
  (:refer-clojure :exclude [get set])
  (:require [com.gfredericks.chess.board :refer :all]
            [com.gfredericks.chess.generators :as cgen]
            [com.gfredericks.chess.squares :as sq]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [simple-check.clojure-test :refer [defspec]]))

(def gen-two-different-squares
  (gen/such-that #(not= (first %) (second %))
                 (gen/tuple cgen/square cgen/square)))

(defspec setting-and-retrieving-a-piece 100
  (prop/for-all [b cgen/board
                 sq cgen/square
                 p cgen/piece]
    (-> b
        (set sq p)
        (get sq)
        (= p))))

(defspec setting-one-and-retrieving-another 100
  (prop/for-all [b cgen/board
                 [sq1 sq2] gen-two-different-squares
                 p1 cgen/piece]
    (let [p2 (get b sq2)
          b' (set b sq1 p1)]
      (and (= p1 (get b' sq1))
           (= p2 (get b' sq2))))))
