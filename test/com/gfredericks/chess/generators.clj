(ns com.gfredericks.chess.generators
  (:refer-clojure :exclude [rand rand-nth shuffle])
  (:require [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.pieces :as pieces]
            [com.gfredericks.chess.position :as position]
            [com.gfredericks.chess.rules :as rules]
            [com.gfredericks.chess.squares :as sq]
            [clojure.test.check.generators :as gen]))

(def square
  (gen/elements sq/all-squares))

(def piece (gen/elements (keys board/pieces)))

(def board
  "Generates a naive board with random entries."
  (gen/fmap
   (fn [pieces]
     (reduce
      (fn [b [piece sq]]
        (board/set b sq piece))
      board/empty-board
      (map list pieces sq/all-squares)))
   (apply gen/tuple (repeat 64 piece))))

(defn ^:private legal-piece-set?
  [pieces]
  (let [{:strs [k q r b n p] :or {k 0 q 0 r 0 b 0 n 0 p 0}}
        (frequencies (map #(-> % name .toLowerCase) pieces)),
        extras (apply + (filter pos? [(dec q)
                                      (- r 2)
                                      (- b 2)
                                      (- n 2)]))]
    (and (= 1 k)
         (<= (+ extras p) 8))))

;; TODO: also check the colors of the bishops?
(defn ^:private legal-position-piece-sets?
  [pos]
  (-> pos
      (:board)
      (board/piece-placements)
      (rules/legal-piece-sets?)))

(defn ^:private legal-position-pawns?
  "Checks if there are any pawns on the back ranks."
  [pos]
  (not-any? (fn [[sq p]]
              (and (#{0 7} (sq/row sq))
                   (#{:p :P} p)))
            (board/piece-placements (:board pos))))

(defn ^:private legal-position-checks?
  [pos]
  ;; just checking that the player to move isn't checking the other
  ;; player for now.
  (let [placements (board/piece-placements (:board pos))
        black-king (->> placements
                        (filter (comp #{:k} second))
                        (ffirst))
        white-king (->> placements
                        (filter (comp #{:K} second))
                        (ffirst))]
    (not (or (and (= :white (:turn pos))
                  (rules/attacks? (:board pos) :white black-king))
             (and (= :black (:turn pos))
                  (rules/attacks? (:board pos) :black white-king))))))

(def ^:private legal-position?
  "Checks if the position is legal wrt its static features:
    - piece set
    - king positions (check)
    - pawn positions"
  (every-pred #'legal-position-pawns?
              #'legal-position-piece-sets?
              #'legal-position-checks?))

(defn ^:private shuffle
  "Return a random permutation of coll"
  [^java.util.Random rnd ^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rnd)
    (clojure.lang.RT/vector (.toArray al))))

(defn rand [^java.util.Random rnd] (.nextDouble rnd))

(defn rand-nth [^java.util.Random rnd coll]
  (nth coll (.nextInt rnd (int (count coll)))))

(defn ^:private generate-castling
  "Returns a castling map for the position, where the entries might be
  true only if the pieces are in the correct positions, otherwise
  always false."
  [^java.util.Random rnd board]
  (letfn [(thing [king rook king-square rook-square]
            (if (and (= king (board/get board king-square))
                     (= rook (board/get board rook-square)))
              (case (.nextInt rnd 2) 0 true 1 false)
              false))]
    {:white {:king  (thing :K :R sq/e1 sq/h1)
             :queen (thing :K :R sq/e1 sq/a1)}
     :black {:king  (thing :k :r sq/e8 sq/h8)
             :queen (thing :k :r sq/e8 sq/a8)}}))

(defn generate-en-passant
  "If the player that just moved could plausibly have made a pawn
  jump, might set the en-passant square to behind any of those
  pawns. Otherwise returns nil."
  [^java.util.Random rnd board player-to-move]
  (let [moved-player ({:black :white :white :black} player-to-move)
        jump-row      (sq/pawn-jump-row moved-player)
        jump-over-row (sq/pawn-jump-over-row moved-player)
        start-row     (sq/pawn-start-row moved-player)
        possibles (for [col (range 8)
                        :let [p (board/get board (sq/square col jump-row))]
                        :when (and (pieces/pawn? p)
                                   (pieces/color? moved-player p))
                        :let [sq (sq/square col jump-over-row)]
                        :when (pieces/blank? (board/get board sq))
                        :when (pieces/blank? (board/get board (sq/square col start-row)))]
                    sq)]
    (rand-nth rnd (cons nil possibles))))

(defn random-position
  [rnd]
  (let [rand-blank (fn [board]
                     (->> sq/all-squares
                          (shuffle rnd)
                          (drop-while #(not= :_ (board/get board %)))
                          (first)))
        rand-pawn-blank (fn [board]
                          (->> sq/all-squares
                               (shuffle rnd)
                               (drop-while
                                (fn [sq]
                                  (or (not= :_ (board/get board sq))
                                      (#{0 7} (sq/row sq)))))
                               (first)))

        place (fn [board piece]
                (board/set board
                           ((if (= :pawn (pieces/piece-type piece))
                              rand-pawn-blank
                              rand-blank) board)
                           piece))
        piece-prob (rand rnd)
        pieces-to-place
        (concat [:q :r :r :b :b :n :n]
                [:Q :R :R :B :B :N :N]
                ;; the repeat at the end makes this pawn-heavy
                (repeat 8 #(rand-nth rnd (list* :n :b :r :q (repeat 10 :p))))
                (repeat 8 #(rand-nth rnd (list* :N :B :R :Q (repeat 10 :P)))))

        board
        (-> board/empty-board
            (place :k)
            (place :K)
            (as-> <>
                  (reduce (fn [board new-piece]
                            (let [new-piece (if (fn? new-piece)
                                              (new-piece)
                                              new-piece)]
                              (cond-> board
                                      (< (rand rnd) piece-prob)
                                      (place new-piece))))
                          <>
                          pieces-to-place)))
        turn (rand-nth rnd [:white :black])
        pos (with-meta {:board board
                        :turn turn
                        :castling (generate-castling rnd board)
                        :en-passant (generate-en-passant rnd board turn)
                        :half-move 0
                        :full-move 0}
              {:type ::position/position})]
    (if (legal-position? pos)
      pos
      (recur rnd))))

(defn position-shrinks
  [pos]
  (lazy-seq
   (->> (board/piece-placements (:board pos))
        (remove (comp pieces/king? second))
        (map (fn [[sq _p]]
               (let [pos' (update-in pos [:board] board/set sq :_)]
                 [pos' (position-shrinks pos')]))))))

(def position
  {:gen (fn [rnd size]
          (let [pos (random-position rnd)]
            [pos (position-shrinks pos)]))})

(defn ^:private gen-position-from-random-game*
  [moves-left pos]
  (if (zero? moves-left)
    (gen/return pos)
    (if-let [moves (seq (rules/moves pos))]
      (gen/let [move (gen/elements moves)]
        (gen-position-from-random-game* (dec moves-left) (rules/make-move pos move)))
      (gen/return pos))))

(def gen-position-from-random-game
  (gen/let [move-count (gen/scale #(* 2 %) gen/nat)]
    (gen-position-from-random-game* move-count position/initial)))

(def gen-two-kings-position
  "Generates a random legal position with just two kings."
  )

#_
(def gen-position-from-random-placements
  (gen/let [[init-pos changes]]))

(def position
  gen-position-from-random-game)
