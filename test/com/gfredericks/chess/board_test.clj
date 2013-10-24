(ns com.gfredericks.chess.board-test
  (:refer-clojure :exclude [get set])
  (:require [com.gfredericks.chess.board :refer :all]
            [simple-check.core :as sc]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [simple-check.clojure-test :refer [defspec]]))

(def gen-square
  (gen/elements all-squares))

(def gen-two-different-squares
  (gen/such-that #(not= (first %) (second %))
                 (gen/tuple gen-square gen-square)))

(def gen-piece (gen/elements (keys pieces)))

(def gen-board
  (gen/fmap
   (fn [pieces]
     (reduce
      (fn [b [piece sq]]
        (set b sq piece))
      empty-board
      (map list pieces all-squares)))
   (apply gen/tuple (repeat 64 gen-piece))))


(defspec setting-and-retrieving-a-piece 100
  (prop/for-all [b gen-board
                 sq gen-square
                 p gen-piece]
    (-> b
        (set sq p)
        (get sq)
        (= p))))

(defspec setting-one-and-retrieving-another 100
  (prop/for-all [b gen-board
                 [sq1 sq2] gen-two-different-squares
                 p1 gen-piece]
    (let [p2 (get b sq2)
          b' (set b sq1 p1)]
      (and (= p1 (get b' sq1))
           (= p2 (get b' sq2))))))
