(ns com.gfredericks.chess.proof-games
  (:refer-clojure :exclude [shuffle rand rand-int])
  (:require [com.gfredericks.chess.board :as board]
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


#_
(defn search*
  [partial-position stack]
  (let [c (cost partial-position)]
    (if (zero? c)
      [{:position partial-position
        :cost 0
        :moves ()}]
      (let [move-tuples
            (->> (rules/unmoves partial-position)
                 (map (fn [idx move]
                        (let [pos' (rules/make-unmove
                                    partial-position
                                    move)]
                          [(cost pos') idx move pos']))
                      (range))
                 (sort))]
        (if (empty? move-tuples)
          )
        )
      ))
  )

(defn create-search
  [partial-position & kvs]
  (let [c (cost partial-position)]
    (condp == c
      0.0
      {:state :done
       :moves []}

      ##Inf
      {:state :unreachable}

      ;; gotta use cond-> because assoc doesn't work for one arg
      (cond-> {:state     :running-greedy
               :rng       (java.util.SplittableRandom. 42)
               :steps     0
               :stack     []
               :position  partial-position
               :cost      (cost partial-position)
               ;; is there a reasonable default here? how many moves do
               ;; we suspect the farthest-to-reach position is?
               :max-moves 200}
        (seq kvs)
        (as-> m (apply assoc m kvs))))))

(defn jusr->jur
  [^java.util.SplittableRandom jusr]
  (proxy [java.util.Random] [0]
    (next [bits]
      (assert (< bits 32))
      (bit-and 0x7FFFFFF (.nextInt jusr)))))

(defn shuffle
  [jusr xs]
  (let [al (java.util.ArrayList. ^java.util.Collection xs)]
    (java.util.Collections/shuffle al (jusr->jur jusr))
    (seq al)))

;; ideas
;; - use hyperloglog to estimate how many unique positions we've seen
;;   (as an alternative to keeping lots of state around to track it)
(defn search-step
  [{:keys [state position stack] :as search}]
  (case state
    :running-greedy
    (let [tuples (->> (rules/unmoves position)
                      (map (fn [unmove]
                             (let [p (rules/make-unmove position unmove)]
                               [unmove p (cost p)])))
                      (remove #(= ##Inf (last %)))
                      ;; TODO: prollably more efficient to just stick a random
                      ;; element in the tuple and do a single sort
                      ;;
                      ;; could even experiment with a lazy sort at
                      ;; some point, where we do a single pass picking
                      ;; out all the best values and then lazily sort
                      ;; the tail the same way or different; but maybe
                      ;; that's dumb I bet this stuff is dwarfed by
                      ;; the other logic we'll be doing
                      (shuffle (:rng search))
                      (sort-by last))]
      (if (or (empty? tuples)
              ;; these two cases will presumably be separated in the future
              (< (:cost search) (last (first tuples))))
        ;; nowhere to go from here, pop the stack
        (if (empty? stack)
          (assoc search :state :gave-up)
          (assoc search :state :stack-pop))
        (let [[[unmove position-2 cost] & more-tuples] tuples]
          (if (zero? cost)
            {:state :done
             :moves (concat (map :unmove-taken stack) [unmove])}
            ;; descend!
            ;; TODO: check max-moves
            (-> search
                (update :stack conj
                        {:starting-position position
                         :unmove-taken unmove
                         :unmove-idx 0
                         :tuples tuples})
                (update :steps inc)
                (assoc :position position-2 :cost cost))))))
    :stack-pop
    (if (empty? stack)
      (assoc search :state :gave-up)
      (let [{:keys [starting-position unmove-idx tuples]} (peek stack)
            unmove-idx (inc unmove-idx)]
        ;; would this kind of situation be easier if we modeled the entire
        ;; search object as a stack, instead of having various top-level
        ;; attributes?
        (if ())))))

(defn run-search
  [search max-steps]
  (if (or (zero? max-steps)
          (#{:gave-up :unreachable :done} (:state search)))
    search
    (recur (search-step search) (dec max-steps))))


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
