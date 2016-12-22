(ns com.gfredericks.chess.proof-games
  (:require [clojure.spec :as s]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.moves :as moves]
            [com.gfredericks.chess.pieces :as pieces]
            [com.gfredericks.chess.position :as position]
            [com.gfredericks.chess.squares :as sq]
            [com.gfredericks.chess.rules :as rules]))

;;
;; Do we really want A* or just something similar?  Cuz we're not
;; going for the shortest route in particular...there's lots of local
;; minima we need to avoid. And phrasing it as distance might be hard.
;;

(def initial-board (:board position/initial))

(defn pieces-out-of-position
  [board]
  (->> (board/piece-placements initial-board)
       (remove (fn [[sq piece]]
                 (= piece (board/get board sq))))
       (count)))

(defn cost
  [{:keys [board]}]
  ;; hey we could attach weights to these and run some kind of
  ;; competition to figure out the best weights :)
  (+ (pieces-out-of-position board)))

(defn search
  [partial-position]
  (let [c (cost partial-position)]
    (if (zero? c)
      [{:position partial-position
        :cost 0
        :moves ()}]))
  )
