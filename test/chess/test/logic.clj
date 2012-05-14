(ns chess.test.logic
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as t :refer [deftest]])
  (:use chess.logic
        [chess.logic board pieces]
        clojure.core.logic))

(def blank-board
  [[:_ :_ :_ :_ :_ :_ :_ :_]
   [:_ :_ :_ :_ :_ :_ :_ :_]
   [:_ :_ :_ :_ :_ :_ :_ :_]
   [:_ :_ :_ :_ :_ :_ :_ :_]
   [:_ :_ :_ :_ :_ :_ :_ :_]
   [:_ :_ :_ :_ :_ :_ :_ :_]
   [:_ :_ :_ :_ :_ :_ :_ :_]
   [:_ :_ :_ :_ :_ :_ :_ :_]])

(t/deftest foo
  (t/is (= 12 (count (run* [p] (pieceo p)))))
  (-> (run 1 [p] (normal-king-moveo [:c 3] p))
      empty?
      not
      t/is))

(t/deftest find-king-test
  (let [board [[:q :n :b :_ :_ :_ :_ :_]
               [:_ :_ :_ :p :p :p :_ :_]
               [:_ :_ :_ :_ :_ :_ :_ :_]
               [:_ :_ :_ :_ :P :P :P :_]
               [:_ :_ :_ :_ :P :K :_ :_]
               [:_ :_ :_ :_ :_ :Q :_ :_]
               [:_ :_ :_ :_ :_ :_ :_ :_]
               [:_ :_ :_ :_ :_ :_ :_ :_]]
        res (run* [q]
                  (board-entryo board q :K))]
    (t/is (= [[:f 4]]))))

(def succeeds? (complement empty?))

(t/deftest queen-move-test
  (t/is (succeeds? (run 1 [q]
                        (normal-queen-moveo blank-board [:b 1] [:e 4])))))

(t/deftest knight-move-test
  (let [sqrs (run* [q] (fresh [x] (normal-knight-moveo [:c 4] q)))]
    (t/is (= (sort sqrs) (sort [[:b 6] [:b 2]
                                [:d 6] [:d 2]
                                [:a 3] [:a 5]
                                [:e 3] [:e 5]])))))