(ns chess.test.logic.board
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as t :refer [deftest]])
  (:use chess.logic.board
        clojure.core.logic))

(def blank-board (zipmap all-squares (repeat :_)))

(t/deftest board-entry-changeo-test
  (t/is (= '(_.0) (run 2 [q] (board-entry-changeo
                              blank-board
                              (assoc blank-board [:e 4] :K)
                              [:e 4]))))
  (t/is (= () (run 2 [q] (board-entry-changeo
                          blank-board
                          (assoc blank-board [:e 4] :K [:e 7] :Q)
                          [:e 4])))))