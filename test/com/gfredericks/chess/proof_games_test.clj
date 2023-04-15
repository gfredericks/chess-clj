(ns com.gfredericks.chess.proof-games-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.results :as results]
            [com.gfredericks.chess.proof-games :as proof-games]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.position :as position]
            [com.gfredericks.chess.moves :as moves]
            [com.gfredericks.chess.rules :as rules]
            [com.gfredericks.chess.rules-test :as rules-test]))

(deftest pieces-out-of-position-test
  (is (= 0 (proof-games/pieces-out-of-position (:board position/initial))))
  (is (= 32 (proof-games/pieces-out-of-position board/empty-board))))

(deftest distances-from-home-test
  (is (= 0 (proof-games/distances-from-home (:board position/initial))))
  (is (= 2 (proof-games/distances-from-home #chess/fen-board "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/1RBQKBNR")))
  (is (= 3 (proof-games/distances-from-home #chess/fen-board "rnbqkbnr/1ppppppp/8/p7/4N3/8/PPPPPPPP/1RBQKBNR")))
  (is (= 2 (proof-games/distances-from-home #chess/fen-board "rn1qkbnr/ppp1pppp/3p4/8/6b1/2N5/PPPPPP1P/R1BQKBNR")))
  ;; remember the blocked out rooks are 7 each
  (is (= 25 (proof-games/distances-from-home #chess/fen-board "2k4r/ppp3p1/2n3b1/2Pr2P1/Q2Pp3/4BqP1/PP5P/2K1RB1R"))))

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
(defn test-solvable-position
  [pos1 max-steps max-moves]
  (let [{:keys [result data total-steps]}
        (proof-games/run-search (proof-games/create-search pos1) max-steps)

        moves-2 (:moves data)]
    (reify results/Result
      (pass? [_]
        (and (= :done result)
             (< (count moves-2) max-moves)
             (let [pos2 (reduce rules/make-move position/initial (reverse moves-2))]
               (= (:board pos1) (:board pos2)))))
      (result-data [_]
        {:position pos1}))))

(defn very-short-games-test
  [pos]
  ;; 100 steps should generally be enough, but unfortunately the
  ;; randomness in our algorithm means there can be a lot of variance;
  ;; should we do something to make the initial search more
  ;; deterministic/optimistic/greedy?

  ;; we could also rewrite this to try different seeds I guess
  (test-solvable-position pos 1000 20))

(defspec very-short-games 100
  (prop/for-all [moves (gen/resize 5 rules-test/gen-game-prefix)]
    (very-short-games-test (reduce rules/make-move position/initial moves))))

(deftest very-short-games-regression
  (doseq [pos [#chess/fen "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/1RBQKBNR w Kkq - 4 3"
               #chess/fen "rnbqkbnr/1ppppppp/8/p7/4N3/8/PPPPPPPP/1RBQKBNR b Kkq - 1 3"
               #chess/fen "rnbqkbnr/p1pppppp/8/1p6/N7/7N/PPPPPPPP/R1BQKB1R b KQkq - 1 3"
               #chess/fen "rnbqkbnr/2pppppp/pp6/8/1P6/2B5/P1PPPPPP/RN1QKBNR b KQkq - 1 3"
               #chess/fen "rn1qkbnr/ppp1pppp/3p4/8/6b1/2N5/PPPPPP1P/R1BQKBNR w KQkq - 0 3"
               #chess/fen "rnbqkbnr/pp1ppppp/8/2p5/2N5/8/PPPPPPPP/R1BQKBNR b KQkq - 1 2"
               #chess/fen "rnbqkbnr/2pppppp/p7/1p6/Q4P2/2P5/PP1PP1PP/RNB1KBNR b KQkq - 1 3"]
          :let [result (very-short-games-test pos)]]
    (is (results/pass? result)
        (str "Can't solve " pos))))

(defn step-through-search
  [pos]
  (loop [{:keys [stack] :as search} (proof-games/create-search pos)
         steps 0]
    (->> (:stack search)
         (rest)
         (run! (fn [{:keys [budget-used cost unmove]}]
                 (printf "%02d %.3f %s\n" budget-used cost
                         (moves/format-humanely unmove)))))
    (prn (assoc (select-keys search [:search-budget :frustration])
                :steps steps))
    (position/print-position (:position (peek stack)))
    (.read System/in)
    (recur (proof-games/search-step search) (inc steps))))

(defn -main
  []
  (step-through-search #chess/fen "k7/8/8/8/8/8/8/7K w - - 4 3")
  #_
  (doseq [pos [#chess/fen "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/1RBQKBNR w Kkq - 4 3"
               #chess/fen "rnbqkbnr/1ppppppp/8/p7/4N3/8/PPPPPPPP/1RBQKBNR b Kkq - 1 3"
               #chess/fen "rnbqkbnr/p1pppppp/8/1p6/N7/7N/PPPPPPPP/R1BQKB1R b KQkq - 1 3"
               #chess/fen "rnbqkbnr/2pppppp/pp6/8/1P6/2B5/P1PPPPPP/RN1QKBNR b KQkq - 1 3"
               #chess/fen "rn1qkbnr/ppp1pppp/3p4/8/6b1/2N5/PPPPPP1P/R1BQKBNR w KQkq - 0 3"
               #chess/fen "rnbqkbnr/pp1ppppp/8/2p5/2N5/8/PPPPPPPP/R1BQKBNR b KQkq - 1 2"
               #chess/fen "rnbqkbnr/2pppppp/p7/1p6/Q4P2/2P5/PP1PP1PP/RNB1KBNR b KQkq - 1 3"]]
    (println "Running" pos)
    (let [{:keys [result data]} (proof-games/run-search
                                 (proof-games/create-search pos)
                                 1000)
          {:keys [moves]} data]
      (if (= :done result)
        (println "good" (count moves))
        (println "bad"))))

  #_
  (step-through-search
   #chess/fen "rnbqkbnr/2pppppp/p7/1p6/Q4P2/2P5/PP1PP1PP/RNB1KBNR b KQkq - 1 3")
  #_
  (Let [p #chess/fen "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/1RBQKBNR w Kkq - 4 3"]
       (time
        (prn (proof-games/run-search (proof-games/create-search p)
                                     2000
                                     {:print? false #_true})))))

(defn slightly-longer-games-test
  [pos]
  (test-solvable-position pos 10000 50))

(defspec slightly-longer-games 20
  (prop/for-all [moves (gen/resize 10 rules-test/gen-game-prefix)]
    (slightly-longer-games-test (reduce rules/make-move position/initial moves))))

(comment
  ;; these don't work yet (take forever)
  (defn even-longer-games-test
    [pos]
    (test-solvable-position pos 10000 200))

  (defspec even-longer-games 20
    (prop/for-all [moves (gen/resize 25 rules-test/gen-game-prefix)]
      (even-longer-games-test (reduce rules/make-move position/initial moves)))))


;; Test ideas
;; - get really good at positions with no promoted pieces first!
;; - basic things
;;   - like two kings in random positions, or with a few extra pieces
;;   - extreme pawn positioning in various patterns
;; - generate random positions that are provably legal
;;   - e.g., if we surround each king such that it can't be in check, and then
;;     spread pieces randomly throughout the rest of the board
;;     - I guess the pawn positions could still make it unsolvable
;;   - or a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; phase-based tests

(deftest solving-phase
  (is (= :initial (proof-games/solving-phase (:board position/initial))))
  (are [board] (= :pawn-knights (proof-games/solving-phase board))
    #chess/fen-board "rnbqkbnr/pppppppp/8/8/8/P7/1PPPPPPP/RNBQKBNR"
    #chess/fen-board "rnbqkbnr/pppppppp/8/8/8/6P1/PPPPPP1P/RNBQKBNR"
    #chess/fen-board "rnbqkbnr/p1pppppp/1p6/8/8/6P1/PPPPPP1P/RNBQKBNR"
    #chess/fen-board "rnbqkbnr/p1pppppp/1p6/8/8/5PP1/PPPPP2P/RNBQKBNR"
    #chess/fen-board "rnbqkbnr/p2ppppp/1pp5/8/8/5PP1/PPPPP2P/RNBQKBNR"
    #chess/fen-board "rnbqkbnr/p2ppppp/1pp5/8/8/2P2PP1/PP1PP2P/RNBQKBNR")

  (are [board] (= :non-pawn-knights (proof-games/solving-phase board))
    #chess/fen-board "rnbqkbnr/p1ppppp1/1p5p/P7/R7/8/1PPPPPPP/1NBQKBNR"
    #chess/fen-board "rnbqkbnr/p1pppp2/1p4pp/P7/R7/8/1PPPPPPP/1NBQKBNR"
    #chess/fen-board "rnbqkbnr/p1pppp2/1p4pp/P7/R5P1/8/1PPPPP1P/1NBQKBNR"
    #chess/fen-board "rnbqkbnr/p1ppp3/1p3ppp/P7/R5P1/8/1PPPPP1P/1NBQKBNR"))

(defn gen-positions-in-phase
  [phase]
  (gen/such-that some?
                 (gen/let [moves rules-test/gen-game-prefix]
                   ;; select the longest set of moves that produces
                   ;; the phase we want, or nil otherwise
                   (try
                     (->> moves
                          (reductions rules/make-move position/initial)
                          (reverse)
                          (map (juxt identity (comp proof-games/solving-phase :board)))
                          (filter #(= phase (second %)))
                          (ffirst))
                     (catch Exception e
                       (throw (ex-info "Ooof" {:moves moves} e)))))
                 {:max-tries 100}))

(defspec pawn-knights-phase 20
  (prop/for-all [position (gen-positions-in-phase :pawn-knights)]
    (test-solvable-position position 10000 100)))

;; this takes forever -- probably time to go into position-cost and specialize it
;; for this phase?
(defspec pawn-knights-uncapturing-phase 20
  (prop/for-all [position (gen-positions-in-phase :pawn-knights-uncapturing)]
    (test-solvable-position (doto position prn) 10000 100)))
