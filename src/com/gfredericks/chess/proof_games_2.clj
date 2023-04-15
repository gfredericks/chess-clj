(ns com.gfredericks.chess.proof-games-2
  "Let's try this again!"
  (:require
   [com.gfredericks.chess.board :as board]
   [com.gfredericks.chess.moves :as moves]
   [com.gfredericks.chess.pieces :as pieces]
   [com.gfredericks.chess.position :as position]
   [com.gfredericks.chess.squares :as sq]
   [com.gfredericks.chess.rules :as rules]))

;; let's just try to get a basic thing going where we can write a cost
;; function and run a test to find local minima

(defprotocol CostFunction
  (cost [this position]
    "Returns a numeric (long) cost."))

(defmacro cost-fn
  [[argname] & body]
  `(reify CostFunction
     (cost [this# ~argname] ~@body)
     clojure.lang.IFn
     (invoke [this# ~argname] (cost this# ~argname))))

(let [placements (board/piece-placements (:board position/initial))]
  (def count-out-of-place-pieces
    (cost-fn
     [position]
     (->> placements
          (remove (set (board/piece-placements (:board position))))
          (count)))))

;; implementing this will pass our first test case, but not our second
(def total-distance-from-start
  (cost-fn
   [position]
   ;; easiest efficient way to implement this is to precompute the
   ;; distance for all possible [sq piece] combinations (about 768 of
   ;; them)
   ))
