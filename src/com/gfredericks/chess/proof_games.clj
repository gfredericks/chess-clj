(ns com.gfredericks.chess.proof-games
  (:refer-clojure :exclude [rand-nth])
  (:require [clojure.test.check.random :as random]
            [com.gfredericks.chess.board :as board]
            [com.gfredericks.chess.moves :as moves]
            [com.gfredericks.chess.pieces :as pieces]
            [com.gfredericks.chess.position :as position]
            [com.gfredericks.chess.proof-games.distances :as distances]
            [com.gfredericks.chess.squares :as sq]
            [com.gfredericks.chess.rules :as rules]))

(def initial-board (:board position/initial))

(defn rand-nth
  "immutable version of clojure.core/rand-nth"
  [rng coll]
  (let [count (count coll)]
    (let [idx (mod (random/rand-long rng) count)]
      (nth coll idx))))

(defn exponential-rand-nth
  [rng prob-of-next coll]
  (if (empty? coll)
    (throw (ex-info "exponential-rand-nth on empty coll" {:coll coll}))
    (loop [x (first coll) xs (next coll) rng rng]
      (if xs
        (let [[r1 r2] (random/split rng)]
          (if (< (random/rand-double r1) prob-of-next)
            (recur (first xs) (next xs) r2)
            x))
        x))))

(defn pieces-out-of-position
  [board]
  (->> (board/piece-placements initial-board)
       (remove (fn [[sq piece]]
                 (= piece (board/get board sq))))
       (count)))

(let [piece-type-counts (fn [board]
                          (->> (board/piece-placements board)
                               (map (fn [[sq piece]]
                                      (if (= :bishop (pieces/piece-type piece))
                                        [piece (sq/square-color sq)]
                                        piece)))
                               (frequencies)))
      initial (piece-type-counts initial-board)]
  (defn promoted-piece-count
    [board]
    (->> (piece-type-counts board)
         (map (fn [[piece-spec count]]
                (max 0 (- count (get initial piece-spec)))))
         (reduce +))))

(defn back-row-pieces-locked-out
  [board]
  ;; TODO:
  ;; - you can lock a rook out if all your pawns are in the
  ;;   correct columns and on the second or third rows
  (if (or
       ;; white bishop locked out by two pawns
       (and (= :P
               (board/get board #chess/square b2)
               (board/get board #chess/square d2))
            (not= :B (board/get board #chess/square c1)))
       (and (= :P
               (board/get board #chess/square e2)
               (board/get board #chess/square g2))
            (not= :B (board/get board #chess/square f1)))
       ;; black bishop locked out by two pawns
       (and (= :p
               (board/get board #chess/square b7)
               (board/get board #chess/square d7))
            (not= :b (board/get board #chess/square c8)))
       (and (= :p
               (board/get board #chess/square e7)
               (board/get board #chess/square g7))
            (not= :b (board/get board #chess/square f8)))
       ;; white pawns are all in place
       (and (apply = :P (for [col (range 8)]
                          (board/get board (sq/square col 1))))
            (or
             ;; assuming the bishop is in place, the rook is allowed
             ;; to be on the knight's square or its own square
             (not-any? #{:R}
                       [(board/get board #chess/square a1)
                        (board/get board #chess/square b1)])
             (not-any? #{:R}
                       [(board/get board #chess/square g1)
                        (board/get board #chess/square h1)])
             (not= :Q (board/get board #chess/square d1))
             (not= :K (board/get board #chess/square e1))))
       ;; black pawns are all in place
       (and (apply = :p (for [col (range 8)]
                          (board/get board (sq/square col 6))))
            (or (not-any? #{:r}
                          [(board/get board #chess/square a8)
                           (board/get board #chess/square b8)])
                (not-any? #{:r}
                          [(board/get board #chess/square g8)
                           (board/get board #chess/square h8)])
                (not= :q (board/get board #chess/square d8))
                (not= :k (board/get board #chess/square e8)))))
    ##Inf
    0.0))

(let [full (->> (board/piece-placements (:board position/initial))
                (map second)
                (frequencies))]
  (defn pieces-missing
    [board]
    (let [counts (->> (board/piece-placements board)
                      (map second)
                      (frequencies))]
      (->> full
           (map (fn [[piece c]]
                  (let [actual (get counts piece 0)]
                    (max 0 (- c actual)))))
           (reduce +)))))

(def home-squares
  "Function from piece to a collection of squares."
  (->> (board/piece-placements initial-board)
       (group-by second)
       (map (fn [[piece sq-piece-pairs]]
              [piece (map first sq-piece-pairs)]))
       (into {})))

(defn distances-from-home
  [board]
  (let [all-distances (distances/all-distances-2 board)]
    (->> (board/piece-placements board)
         (remove (fn [[sq piece]]
                   (= :pawn (pieces/piece-type piece))))
         (group-by (fn [[sq piece]]
                     (let [t (pieces/piece-type piece)]
                       [(pieces/piece-color piece)
                        t
                        (if (= :bishop t) (sq/square-color sq) nil)])))
         ;; how do we account for piece multiplicity?  the
         ;; Correct answer is, when there's just one of two
         ;; pieces, try both spots and return the smaller one;
         ;; when both pieces are on the board, try both
         ;; combinations and return the smaller sum
         ;;
         ;; oh we also maybe need to account for promoted
         ;; pieces! unless we enforce phases of the game where
         ;; we don't even consider this stuff until the pieces
         ;; are unpromoted, which I think we probably should;
         ;; there's just too many other considerations
         ;; otherwise
         (map (fn [[[color type bishop-type] sq-piece-tuples]]
                (let [piece (->> sq-piece-tuples first second)
                      ;; step 1: determine which starting squares matter
                      homes (cond->> (home-squares piece)
                              (= :bishop type)
                              (filter #(= bishop-type (sq/square-color %))))
                      ;; step 2: evaluate all possible assignments of
                      ;; pieces to home squares and return the minimum
                      ;; sum-of-distances
                      [sq-p-1 sq-p-2] sq-piece-tuples
                      [home-1 home-2] homes
                      ;; each assignment is a map from [sq piece] to home-square
                      assignments (case [(count sq-piece-tuples) (count homes)]
                                    [1 1]
                                    [{sq-p-1 home-1}]
                                    [1 2]
                                    [{sq-p-1 home-1} {sq-p-1 home-2}]
                                    [2 2]
                                    [{sq-p-1 home-1, sq-p-2 home-2}
                                     {sq-p-1 home-2, sq-p-2 home-1}]
                                    ;; if we have promoted pieces we might get here;
                                    ;; should arrange the algorithm so that doesn't
                                    ;; happen
                                    (throw (ex-info "Shouldn't be possible" {:board board
                                                                             :sq-piece-tuples sq-piece-tuples
                                                                             :homes homes})))]
                  (->> assignments
                       (map (fn [assignment]
                              (->> assignment
                                   (map (fn [[[square piece] home]]
                                          (or (all-distances (pieces/piece-type piece)
                                                             home
                                                             square)
                                              7)))
                                   (reduce +))))
                       (apply min)))))
         (reduce +))))

(defn pawn-behinds
  "Given a piece-color and a square, returns all of the squares
  behind that pawn back to its home square; e.g., for white f4,
  return f3,f2."
  [piece-color sq]
  (let [col (sq/col sq)
        behind-rows
        (case piece-color
          :white (range (sq/pawn-start-row :white) (sq/row sq))
          :black (range (sq/pawn-start-row :black) (sq/row sq) -1))]
    (map (fn [row] (sq/square col row)) behind-rows)))

(defn sad-pawns
  "Tries to incentivize healthy pawn structure"
  [board]
  ;; first try: count the pawns that have any pawns behind them
  (let [all-pawns (->> (board/piece-placements board)
                       (filter (fn [[sq piece]] (= :pawn (pieces/piece-type piece)))))
        squares-with-pawns (->> all-pawns (map first) set)]
    (->> all-pawns
         (filter (fn [[sq piece]]
                   (->> (pawn-behinds (pieces/piece-color piece) sq)
                        (filter squares-with-pawns)
                        (first))))
         (count))))

;; okay I think we need to switch this to some well-defined phases;
;; 1) unpromote pieces (TODO)
;;    - distance of promoted pieces from back rank
;;      - possibly develop an opinion about the optimal
;;        depromotation square
;;    - pawn-sadness
;;    - pieces on board -- don't incentivize uncapturing
;;      until phase 3!
;; 2) untangle white vs black (no black pieces behind white pieces)
;;    - pieces on board
;;    - pawn sadness
;; 3) uncapture all non-pawns
;; 4) move all non-pawn-knights to home squares
;; 5) uncapture pawns with knights on appropriate columns
;; 6) return knights and pawns to home rows

(let [not-pawn-knight-placements (->> (board/piece-placements initial-board)
                                      (remove (comp #{:knight :pawn}
                                                    pieces/piece-type
                                                    second)))
      npkp-sqs (set (map first not-pawn-knight-placements))
      pawn-knight-placements (->> (board/piece-placements initial-board)
                                  (filter (comp #{:knight :pawn}
                                                pieces/piece-type
                                                second)))]
  (defn solving-phase
    [board]
    (let [placements (delay (board/piece-placements board))
          separated?
          (delay
            (let [get-rows (fn [color]
                             (->> @placements
                                  (filter #(= color (pieces/piece-color (second %))))
                                  (map (comp sq/row first))))]
              (<= (apply max (get-rows :white))
                  (apply min (get-rows :black)))))]
      (if (every? (fn [[sq piece]] (board/at? board sq piece))
                  not-pawn-knight-placements)
        (if (every? (fn [[sq piece]] (board/at? board sq piece))
                    pawn-knight-placements)
          ;; TODO: we can't here at the moment (don't know whose turn
          ;; it is) but somewhere we'll need to care about the turn
          ;; parity problem with just knights out
          :initial
          ;; oh this is tricky; consider e4,d5,exd5; do we need to recognize
          ;; when a position is solvable? sounds hard; can save that for later
          ;; I guess
          (let [other-placements (remove (comp npkp-sqs first) @placements)]
            ;; LOL this code doesn't recognize when you have extra
            ;; promoted knights
            (if (->> other-placements
                     (every? (fn [[sq piece]]
                               (or (pieces/knight? piece)
                                   (and (pieces/pawn? piece)
                                        (->> (pawn-behinds (pieces/piece-color piece) sq)
                                             (not-any? (fn [sq]
                                                         (pieces/pawn? (board/get board sq))))))))))
              (if (= 20 (count other-placements))
                :pawn-knights
                :pawn-knights-uncapturing)
              :TODO
              )))))))

(defn position-cost
  [{:keys [board]}]
  ;; hey we could attach weights to these and run some kind of
  ;; competition to figure out the best weights :)

  ;; TODO: we could short-circuit as soon as any of these is ##Inf
  (let [ppc (promoted-piece-count board)]
    (if (pos? ppc)
      ;; TODO: in the promoted piece phase we'll want a distances-from
      ;; depromation-square or something like that; will be hard to
      ;; privilege the more convenient columns...
      (* ppc 10000)
      (+ (pieces-out-of-position board)
         (distances-from-home board)
         (* 3 (pieces-missing board))
         (back-row-pieces-locked-out board)
         (* 50 (sad-pawns board))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; try something totally different!

(defn create-search
  ([position] (create-search position {}))
  ([position {:keys [seed] :or {seed 42}}]
   { ;; budget is the number of not-improving steps we allow; the budget
    ;; will gradually increase as the search faces difficulties
    ;; (when though? only from the root node? if we're searching a very
    ;; long game it feels like it'd be silly to end up backing up to the
    ;; beginning...; maybe we'll have to formalize "phases" at some point)
    :state         :searching
    :search-budget 1
    :rng           (random/make-random seed)
    ;; represents probability of backing up an additional step after
    ;; hitting a wall; gets increased somehow
    :frustration   0.01
    :stack         (let [the-cost (position-cost position)]
                     [{:position position
                       :cost the-cost
                       :improvements [the-cost]
                       :budget-used 0}])}))

(defn search-step
  [{:keys [search-budget stack rng frustration] :as search}]
  ;; when do we decide to unravel (only when no moves available?) and
  ;; how far do we unravel in that case? should we have a frustration
  ;; parameter that's gradually increasing and influences how far up
  ;; we go? could start with that I guess...and when we go up to the
  ;; top, that's when we increase the budget
  (let [{:keys [position cost budget-used improvements]} (peek stack)
        budget-available (- search-budget budget-used)
        unmoves (->> (rules/unmoves position)
                     (map (fn [unmove]
                            (let [pos-2 (rules/make-unmove position unmove)]
                              ;; TODO: the (pr-str unmove) here is probably
                              ;; bad for perf; what's the fastest way to get
                              ;; a deterministic ordering? I don't enjoy the
                              ;; idea of messing with the defrecords in the
                              ;; moves ns to get them to be Comparable but
                              ;; maybe that's the best?
                              [(position-cost pos-2) (pr-str unmove) unmove pos-2])))
                     (sort)
                     (map (fn [[a _ b c]] [a b c])))
        last-improvement (peek improvements)
        unmoves (cond->> unmoves
                  ;; if no budget available, then only look at the good moves
                  (not (pos? budget-available))
                  (take-while (fn [[cost' pos']] (< cost' last-improvement))))]
    (if (empty? unmoves)
      (if (empty? (pop stack))
        ;; crap I think we're done then
        {:state :impossible}

        (let [[r1 r2] (random/split rng)
              popped-stack (->> (iterate pop stack)
                                (next) ;; have to pop at least one frame
                                (take-while seq) ;; don't pop all the way to empty
                                (exponential-rand-nth r1 frustration))]
          (if (next popped-stack)
            ;; we're not all the way back to the beginning
            (-> search
                (assoc :rng r2 :stack popped-stack)
                (update :frustration (let [inv #(- 1 %)]
                                       #(-> % inv (* 0.9) inv))))
            ;; we ARE all the way back to the beginning! geez what a
            ;; tough search
            (-> search
                (assoc :rng r2 :frustration 0.01 :stack popped-stack)
                (update :search-budget inc)))))
      (if (zero? (ffirst unmoves))
        ;; hooray we're done!
        {:state :done
         :moves (->> (conj stack {:unmove (second (first unmoves))})
                     (rest)
                     (map :unmove))}
        (let [[r1 r2 r3 r4] (random/split-n rng 4)
              chosen (->> unmoves
                          (partition-by first)
                          (exponential-rand-nth r1 0.6666666667)
                          (rand-nth r2))
              [chosen-cost chosen-unmove pos-2] chosen
              improvement? (< chosen-cost (peek improvements))]
          (-> search
              (assoc :rng r4)
              (update :stack conj
                      {:position pos-2
                       :cost chosen-cost
                       :budget-used (if improvement? budget-used (inc budget-used))
                       :unmove chosen-unmove
                       :improvements (cond-> improvements
                                       improvement? (conj chosen-cost))})))))))

(defn run-search
  ([search max-steps]
   (run-search search max-steps {}))
  ([search max-steps {:keys [print?] :or {print? false}}]
   (loop [{:keys [state] :as search} search
          total-steps 0]
     (cond (#{:done :impossible} state)
           {:result state :data search :total-steps total-steps}

           (= max-steps total-steps)
           {:result :max-steps :search search :steps total-steps}

           :else
           (do
             (when print?
               (if-let [p (-> search :stack peek :position)]
                 (position/print-position p)
                 (throw (ex-info "bad search????" {:search search}))))
             (recur (search-step search) (inc total-steps)))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; old notes

  ;; how do we back up a level?
  ;; maybe I should sketch out an algorithm on paper and identify
  ;; the different situations we need to account for
  ;;
  ;; let's say we reset with probability proportional to
  ;; 1/sqrt(stepssofar)
  ;;
  ;; what if we're okay with keeping a stack of frames, where in
  ;; each frame we keep track of what moves we've tried so we
  ;; don't repeat them; then maybe the idea would be that at any
  ;; point we have some probability of skipping some frames up the
  ;; stack? we should only do it if we haven't been making
  ;; progress in some sense; maybe we'd have the score in each
  ;; frame as well so the idea would be that you jump back to a
  ;; point where the score was better? that would prevent us from
  ;; jumping back when we're at an all time high; if you think of
  ;; the score as a mountain range, the idea would be that you
  ;; would only jump back to parts that you can see from where you
  ;; are
  ;;
  ;; okay well that's interesting, so then the other question is
  ;; how do we pick moves? I guess pick the best move by score,
  ;; otherwise in a random order, and just walk through them
  ;; tree-search fashion
  ;;
  ;; we should start with that and add jumping later, probably?
  ;; it could get into a really deep line pretty easily I think,
  ;; without something to discourage that; also do we want to
  ;; prevent cycles in the algorithm somehow?
  ;;
  ;; do we need some kind of "tolerance for worseness" parameter
  ;; that gradually increases as we have trouble?
  ;;
  ;; damn I thought this was going to be the easy part, really
  ;;

  ;; okay okay okay okay start over; so here's how we *begin*:
  ;; - greedily follow the best move until things are no longer
  ;;   improving; at this point we're at a local minimum and need
  ;;   to decide what to do; obvs we could keep searching from
  ;;   *some* node; there will have been a lot of random choices
  ;;   we made because more than one option had the same best
  ;;   score, in which case we could obviously go back up and
  ;;   explore those
  ;;   - let's just start with this! an algorithm that explores
  ;;     the full tree of improving moves; then we'll have
  ;;     something that works in some cases, we can write tests,
  ;;     and then look at the simplest case that we can't solve
  ;;     and decide what to work on next
