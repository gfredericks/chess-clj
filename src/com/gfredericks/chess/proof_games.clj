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

(let [initial (->> (board/piece-placements initial-board)
                   (map second)
                   (frequencies))]
  (defn promoted-piece-count
    [board]
    (->> (board/piece-placements board)
         (map second)
         (frequencies)
         (map (fn [[piece count]]
                (max 0 (- count (get initial piece)))))
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
      (* ppc 1000)
      (+ (pieces-out-of-position board)
         (distances-from-home board)
         (pieces-missing board)
         (back-row-pieces-locked-out board)))))

(defn create-search
  [partial-position & kvs]
  (let [c (position-cost partial-position)]
    (condp == c
      0.0
      [{:state :done
        :moves []}]

      ##Inf
      [{:state :unreachable}]

      ;; gotta use cond-> because assoc doesn't work for one arg
      [(cond-> {:state    :hill-climb-start
                :rng      (random/make-random 42)
                :position partial-position
                :cost     c}
         (seq kvs)
         (as-> m (apply assoc m kvs)))])))

(defn max-cost-from-stack
  [stack]
  ;; just a heuristic I'm making up for now
  (let [x (count stack)]
    (if (< x 3)
      ##Inf
      (->> (subvec stack 0 (- x 3))
           (keep :cost)
           (apply min ##Inf)))))

;; ideas
;; - use hyperloglog to estimate how many unique positions we've seen
;;   (as an alternative to keeping lots of state around to track it)
(defn search-step
  [stack]
  (let [top (peek stack)]
    #_
    (printf "SEARCH STEP[%s][%d]\n"
            (mapv :state stack)
            (count stack))
    (case (:state top)
      :hill-climb-start
      (let [position (:position top)
            [r1 r2] (random/split (:rng top))
            unmoves (rules/unmoves position)
            max-cost-exclusive (max-cost-from-stack stack)
            tuples (->> unmoves
                        (map (fn [r unmove]
                               (let [p (rules/make-unmove position unmove)]
                                 [(position-cost p) (random/rand-long r) unmove p]))
                             (random/split-n r1 (count unmoves)))
                        (remove #(<= max-cost-exclusive (first %)))
                        (sort)
                        (map (fn [[cost _ unmove p]] [cost unmove p])))]
        (-> stack
            pop
            (conj (assoc top :state :hill-climb-running :move-tuples tuples :rng r2))))

      :hill-climb-running
      (let [tuples (:move-tuples top)]
        (if (empty? tuples)
          (let [stack' (pop stack)]
            (if (empty? stack')
              [{:state :gave-up}]
              stack'))
          (let [[cost unmove p] (first tuples)
                [r1 r2] (random/split (:rng top))]
            #_
            (printf "TRYING %s (cost=%s) from cost=%s, stacksize=%s\n"
                    (print-str unmove) cost (:cost (peek stack)) (count stack))
            ;; we could avoid checking this for each move (since
            ;; they're already sorted) by being more clever, but
            ;; probably not worth it
            (if (zero? cost)
              [{:state :done :moves (cons unmove (reverse (keep :move stack)))}]
              (-> stack
                  pop
                  (conj (-> top (update :move-tuples next) (assoc :rng r1))
                        {:state :hill-climb-start
                         :move unmove
                         :position p
                         :rng r2
                         :cost cost})))))))))

;; ANOTHER IDEA
;; - is there something better than a proper tree search?
;; - I dunno; I do think we need to keep it "async", i.e. modeling
;;   the search stack explicitly, because that will be necessary
;;   for js animation anyhow
;;   - it feels like it'd be useful to be able to express subgoals
;;     and find out about the results of those goals and react to
;;     them, just like you call a function and get the return value
;;     and decide where to go next; but modeling that with pure data
;;     seems pretty tedious
;;     - can I build a let macro that does this? seems crazy, but...
;;     - I think I'm confused about whether the basic unit of computation
;;       here acts on a whole stack or just the top of the stack, which
;;       is a state in a certain sense
;;       - would it be weird to attach the "return" value from a subsearch
;;         as a key on a frame?
;;         - does this suggest an easy way to implement a let macro?
;;         - so suppose we try to implement a framework where the basic
;;           primitive is functions on the current state, which is at the
;;           top of the stack; what would the semantics be?
;;           - [:push new-frame], [:return value], and [:replace new-frame]?
;;             - is that too many? is push and return simpler?
;;           - um; can we implement clojure's special forms? erhm
;; - what about producing lazy seqs of states, instead of having a stack;
;;   is that crazy? it might make the async stuff easier, arguably; it makes
;;   aborting a calculation from above easier. does it make getting return
;;   values easier?
;;   - it *does* mean that the state of the computation will be implicit in
;;     the runtime's call stack. Which is maybe fine?
;;   - it might be prone to absurd stack-thrashing though

(defn run-search
  [search max-steps]
  (loop [search search
         total-steps 0]
    (cond (= max-steps total-steps)
          {:result :max-steps :search search :steps total-steps}

          (#{:gave-up :unreachable :done} (:state (peek search)))
          {:result (:state (peek search)) :data (peek search) :steps total-steps}

          :else
          (recur (search-step search) (inc total-steps)))))


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
