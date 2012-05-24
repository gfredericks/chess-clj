(ns chess.test.logic.board
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as t :refer [deftest]])
  (:use chess.logic.board
        clojure.core.logic))

(def blank-board (vec (repeat 8 (vec (repeat 8 :_)))))

(t/deftest board-entry-changeo-test
  (t/is (= [[:a 1]] (run* [q]
                        (fresh [x y]
                               (board-entry-changeo
                                (matrix->tree blank-board)
                                (matrix->tree (update-in blank-board [7 0] :K))
                                q
                                x
                                y)))))
  (t/is (= 64 (count (run* [q] (fresh [a b c d e]
                                      (board-entry-changeo a b c d e)
                                      (== q [a b c d e])))))))