(ns com.gfredericks.chess.proof-games-2-test
  (:require
   [clojure.test :refer [are deftest is]]
   [clojure.test.check :refer [quick-check]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.results :as results]
   [com.gfredericks.chess.proof-games-2 :as nsut]
   [com.gfredericks.chess.position :as position]
   [com.gfredericks.chess.rules :as rules]
   [com.gfredericks.chess.rules-test :as rules-test]))

(defn find-path-back-to-start
  "Returns nil if no path can be found."
  [cost-function position]
  (if (= position/initial position)
    []
    (let [cost (nsut/cost cost-function position)]
      (->> (rules/unmoves position)
           (keep (fn [unmove]
                   (let [pos' (rules/make-unmove position unmove)
                         cost' (nsut/cost cost-function pos')]
                     (when (< cost' cost)
                       (when-let [path (find-path-back-to-start cost-function pos')]
                         (cons unmove path))))))
           (first)))))

(defn test-no-local-minima
  [cost-function num-tests & qc-args]
  (apply
   quick-check
   num-tests
   (prop/for-all [moves rules-test/gen-game-prefix]
     (loop [pos position/initial
            moves moves
            cost (nsut/cost cost-function position/initial)]
       (if (empty? moves)
         true
         (let [pos' (rules/make-move pos (first moves))
               cost' (nsut/cost cost-function pos')]
           (if (<= cost' cost)
             (if (find-path-back-to-start cost-function pos')
               (recur pos' (rest moves) cost')
               ;; found a local minima!
               false)
             (recur pos' (rest moves) cost'))))))
   qc-args))

(deftest count-out-of-place-pieces-test
  (is (zero? (nsut/cost nsut/count-out-of-place-pieces position/initial)))
  (is (= 3 (nsut/cost nsut/count-out-of-place-pieces
            #chess/fen "rnbqkbnr/p1pppppp/8/1p6/N7/7N/PPPPPPPP/R1BQKB1R b KQkq - 1 3"))))

(def local-minima-test-cases
  [;; simplest position where a piece is two moves away from its
   ;; starting point
   #chess/fen "rnbqkbnr/1ppppppp/p7/8/4N3/8/PPPPPPPP/R1BQKBNR b KQkq - 1 2"
   ;; white moved its knight back to distance-1 from the start, so we
   ;; need to notice that it has to move farther from the start first
   #chess/fen "rnbqkbnr/1ppppppp/8/p7/8/2N5/PPPPPPPP/R1BQKBNR b Kkq - 1 3"])

(def candidate-cost-function
  nsut/count-out-of-place-pieces)

(deftest test-candidate-cost-function
  (is (every? #(find-path-back-to-start candidate-cost-function %)
              local-minima-test-cases))
  (is (:pass? (test-no-local-minima candidate-cost-function 200))))
