(ns com.gfredericks.chess.proof-games-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.chess.proof-games :as proof-games]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.position :as position]
            [com.gfredericks.chess.rules :as rules]
            [com.gfredericks.chess.rules-test :as rules-test]))

(deftest pieces-out-of-position-test
  (is (= 0 (proof-games/pieces-out-of-position (:board position/initial))))
  (is (= 32 (proof-games/pieces-out-of-position board/empty-board))))

;; cool, stack overflow.
(defspec very-short-games 100
  (prop/for-all [moves (gen/resize 5 rules-test/gen-game-prefix)]
    (let [pos1 (reduce rules/make-move position/initial moves)
          {moves-2 :moves :keys [state]} (proof-games/run-search
                                          (proof-games/create-search pos1)
                                          100)]
      (and (= :done state)
           (some? moves-2)
           (let [pos2 (reduce rules/make-move position/initial moves-2)]
             (= (:board pos1) (:board pos2)))))))
