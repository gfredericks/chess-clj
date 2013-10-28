(ns com.gfredericks.chess.bench
  (:require [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.rules :as rules]
            [criterium.core :refer [bench quick-bench]]))

(def test-pos #chess/fen "8/8/5Q1B/2pkp2p/P2p3p/2K4p/p1n3rp/rb5B w - - 0 1")

(bench (rules/position-status test-pos))
;; 2.59 ms

(bench (count (rules/moves test-pos)))
;; 19.5 ms
