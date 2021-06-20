(ns com.gfredericks.chess.proof-games-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.results :as results]
            [com.gfredericks.chess.proof-games :as proof-games]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.position :as position]
            [com.gfredericks.chess.rules :as rules]
            [com.gfredericks.chess.rules-test :as rules-test]))

(deftest pieces-out-of-position-test
  (is (= 0 (proof-games/pieces-out-of-position (:board position/initial))))
  (is (= 32 (proof-games/pieces-out-of-position board/empty-board))))

;; Alrighty, we're getting a :gave-up on this position, because black
;; got itself back into the starting position and so the only way forward
;; is to temporarily regress; a probably-bad idea is for each search step
;; to generate *two* moves, and so we'd have to calculate costs for every
;; pair of moves, which could be pretty crazy. maybe better to have some
;; logic where cost-increase is tolerated for one move. I think that would
;; be more generally useful anyhow
;;
;; #chess/fen "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/1RBQKBNR w Kkq - 4 3"
;;
;; another issue is that once a piece is two moves away from its
;; starting position then we can't make progress, given the current
;; cost function.  we could do something with precalculated distances
;; between pairs of squares, though that might run into the fungibility
;; problem...I dunno maybe not?
(defspec very-short-games 100
  (prop/for-all [moves (gen/resize 5 rules-test/gen-game-prefix)]
    (let [pos1 (reduce rules/make-move position/initial moves)
          {:keys [result data]} (proof-games/run-search
                                 (proof-games/create-search pos1)
                                 100)
          moves-2 (:moves data)]
      (reify results/Result
        (pass? [_]
          (and (= :done result)
               (some? moves-2)
               (let [pos2 (reduce rules/make-move position/initial moves-2)]
                 (= (:board pos1) (:board pos2)))))
        (result-data [_]
          {:position pos1})))))
