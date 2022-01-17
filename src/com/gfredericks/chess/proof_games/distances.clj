(ns com.gfredericks.chess.proof-games.distances
  (:require
   [clojure.set :as set]
   [com.gfredericks.chess.board :as board]
   [com.gfredericks.chess.rules :as rules]
   [com.gfredericks.chess.squares :as sq])
  (:import
   (java.util Arrays)))


(def movement-graphs
  (letfn [(go [deltas ray?]
            (into-array Object
                        (for [sq sq/all-squares]
                          (vec
                           (for [[drow dcol] deltas
                                 :let [ray (as-> (iterate #(sq/translate % drow dcol) sq) <>
                                             (rest <>)
                                             (cond->> <> (not ray?) (take 1))
                                             (take-while some? <>)
                                             (vec <>))]
                                 sq2-idx (range (count ray))]
                             {:sq (get ray sq2-idx)
                              :intermediate (subvec ray 0 sq2-idx)})))))]
    {:king   (go rules/all-standard-movements false)
     :queen  (go rules/all-standard-movements true)
     :rook   (go rules/rectilinear-movements true)
     :bishop (go rules/diagonal-movements true)
     :knight (go rules/knight-moves false)}))

(comment
  ;; two ways to make this smaller
  ;; A) only consider trips that start at the start square for the piece
  ;; B) dedupe by squares-required-to-be-vacant
  (def all-short-trips
    (time
     (let [max-distance 4]
       (into {}
             (for [[piece graph] movement-graphs]
               [piece
                (loop [trips (into {}
                                   (for [sq sq/all-squares
                                         {to-sq :sq, :keys [intermediate]} (aget graph sq)]
                                     ;; TODO change these sets to long-based bitboards
                                     [[sq to-sq 1] [(set intermediate)]]))
                       next-distance 2]
                  (if (= (inc max-distance) next-distance)
                    trips
                    (recur
                     (merge-with into
                                 trips
                                 (into {}
                                       (for [from-sq sq/all-squares
                                             to-sq sq/all-squares
                                             :when (not= from-sq to-sq)]
                                         [[from-sq to-sq next-distance]
                                          (distinct
                                           (for [via sq/all-squares
                                                 :when (and (not= via from-sq) (not= via to-sq))
                                                 leg-1-length (range 1 next-distance)
                                                 :let [leg-2-length (- next-distance leg-1-length)]
                                                 leg-1 (get trips [from-sq via leg-1-length])
                                                 leg-2 (get trips [via to-sq leg-2-length])
                                                 :let [full (conj (set/union leg-1 leg-2) via)]
                                                 ;; check that there's not a shorter path using a subset
                                                 ;; of these squares
                                                 :when (not-any? (fn [shorter-path] (set/subset? shorter-path full))
                                                                 (mapcat (fn [length]
                                                                           (get trips [from-sq to-sq length]))
                                                                         (range 1 next-distance)))]
                                             full))])))
                     (inc next-distance))))])))))

  (doseq [[piece trips] all-short-trips]
    (println (format "TOTAL TRIP COUNT FOR %s:" piece))
    (println (count (apply concat (vals trips))))))

;; assumed implementation detail for efficiency
(assert (= (range 64) sq/all-squares))

;; TODO write this in an incremental way
(defn all-distances
  "Returns a function from piece type, from-square, to-square
  to a distance, or nil if unreachable via open squares. Return
  value is independent of whether from-square or to-square are
  occupied."
  [board]
  (let [infinity (byte 127)
        ;; this array tracks the distances between two empty
        ;; squares; any occupied square has infinite 127 distance
        ;; to everywhere

        arr (doto (make-array Byte/TYPE (* 5 64 64 64))
              (Arrays/fill infinity))
        ;; k represents the highest square allowed in the path
        mkidx (fn [piece from to k]
                (-> (case piece :king 0 :queen 1 :rook 2 :bishop 3 :knight 4)
                    (bit-shift-left 6)
                    (bit-xor k)
                    (bit-shift-left 6)
                    (bit-xor from)
                    (bit-shift-left 6)
                    (bit-xor to)))
        get (fn [piece from to k]
              (aget arr (mkidx piece from to k)))
        set (fn [piece from to k val]
              (aset arr (mkidx piece from to k) (byte val)))
        empty-squares (->> sq/all-squares
                           (filterv #(= :_ (board/get board %))))
        empty-squares-set (clojure.core/set empty-squares)]
    ;; note this takes twice as long as necessary since we're
    ;; pretending it's a directed graph
    (let [k 0
          zero-unoccupied? (empty-squares-set k)]
      (doseq [[piece graph] movement-graphs]
        (doseq [sq empty-squares]
          (set piece sq sq k 0))
        (doseq [sq empty-squares
                {to-sq :sq :keys [intermediate]} (aget graph sq)
                :when (empty-squares-set to-sq)
                ;; this dependency on the intermediate squares for the
                ;; very adjacency graph makes it hard to imagine doing
                ;; this incrementally
                ;;
                ;; there's probably a fast way to update the adjacency
                ;; graph using set differences, with a precalculate
                ;; set of blocked moves for each square, not sure if
                ;; this would help;; ooh we could maybe use a fancy
                ;; 64*64 bitset
                :when (every? empty-squares-set intermediate)]
          (set piece sq to-sq k 1))
        (when zero-unoccupied?
          (let [empty-adjacent (->> (aget graph k)
                                    (map :sq)
                                    (filterv empty-squares-set ))]
            (doseq [from-sq empty-adjacent
                    to-sq empty-adjacent
                    :let [current (get piece from-sq to-sq k)]]
              (set piece from-sq to-sq k (min 2 current)))))))
    (doseq [piece (keys movement-graphs)
            k (range 1 64)
            :let [k-1 (dec k)]
            from-sq empty-squares
            to-sq empty-squares
            :let [current (get piece from-sq to-sq k-1)
                  first-leg (get piece from-sq k k-1)
                  second-leg (get piece k to-sq k-1)
                  total (if (or (= infinity first-leg)
                                (= infinity second-leg))
                          infinity
                          (+ first-leg second-leg))]]
      (set piece from-sq to-sq k (min current total)))
    (with-meta
      (fn [piece from-sq to-sq]
        ;; TODO: do something different if either square is occupied
        (let [val (get piece from-sq to-sq 63)]
          (when (not= infinity val)
            val)))
      {:locals (-/locals)})))

(defn all-distances-2
  "Maybe not that hard to compute on-demand; should at least be a lot
  better than the above."
  [board]
  ;; TODO initialize a blank? cache out here probably
  (fn [piece-type from-sq to-sq]
    ;; 1. check if they're the same square (return 0)
    ;; 2. take one step from from-sq; if to-sq appears, return 1
    ;; 3. take one step back from to-sq; if intersects prior set, return 2
    ;; 4. take one step from each from-sq neighbor BUT ONLY TO NEW SQUARES;
    ;;    if intersects tq-so neighbors, return 3
    ;; 5. either just return 4 at this point, or take one step back from each
    ;;    to-sq neighbor and then return 4 or 5 based on that; we should make
    ;;    this optional so we can turn it on and off
    (if (= from-sq to-sq)
      0
      (let [graph (movement-graphs piece-type)
            blank? #(= :_ (board/get board %))
            allowed-move? (fn [{:keys [sq intermediate]}]
                            (every? blank? intermediate))
            from-sq+1* (->> (aget graph from-sq)
                            (filter allowed-move?)
                            (map :sq))]
        (if (some #{to-sq} from-sq+1*)
          1
          (let [from-sq+1 (set (filter blank? from-sq+1*))]
            (if (empty? from-sq+1)
              nil ;; unreachable
              (let [to-sq+1 (->> (aget graph to-sq)
                                 (filter (comp blank? :sq))
                                 (filter allowed-move?)
                                 (map :sq)
                                 (set))]
                (cond (some #(and (contains? from-sq+1 %) (blank? %)) to-sq+1) 2
                      (empty? to-sq+1) nil ;; unreachable
                      :else
                      (let [from-sq+2 (->> from-sq+1
                                           (mapcat (fn [sq]
                                                     (->> (aget graph sq)
                                                          (filter (comp blank? :sq))
                                                          (remove (comp (conj from-sq+1 from-sq) :sq))
                                                          (filter allowed-move?)
                                                          (map :sq))))
                                           (set))]
                        (cond (some #(contains? from-sq+2 %) to-sq+1) 3
                              (empty? from-sq+2) nil ;; unreachable
                              :else
                              ;; TODO: optionally take a step back from to-sq+1
                              4)))))))))))

(defn -main
  []
  (doseq [[piece graph] movement-graphs
          :let [starting-square (case piece :king 4 :queen 3 :bishop 2 :knight 1 :rook 0)]]
    (printf "\n==== %s ====\n" piece)
    (loop [moves 1
           best-trips {} ;; map from end-sq to set of sets of intermediates, none of which are subsets
           trips [[[starting-square [starting-square] #{}]]]]
      (when (< moves 5)
        (let [new-trips (for [[sq trip-so-far existing-intermediate] (peek trips)
                              {to-sq :sq :keys [intermediate]} (aget graph sq)
                              :let [trip-so-far (conj trip-so-far to-sq)]
                              :when (apply distinct? trip-so-far)
                              ;; I *think* adding sq here is appropriate
                              :let [intermediate (conj (into existing-intermediate intermediate) sq)]
                              :when (not-any? (fn [other-intermediates]
                                                (clojure.set/subset? other-intermediates intermediate))
                                              (get best-trips to-sq))]
                          [to-sq trip-so-far intermediate])
              trips (conj trips new-trips)]
          (printf "Got %d paths of length %d\n" (count (peek trips)) moves)
          (printf "E.g.: %s\n" (-> trips peek seq rand-nth second (->> (map sq/format-square) pr-str)))
          (flush)
          (recur (inc moves)
                 (reduce (fn [best-trips [to-sq moves intermediates]]
                           (update best-trips to-sq conj intermediates))
                         best-trips
                         new-trips)
                 trips))))))
