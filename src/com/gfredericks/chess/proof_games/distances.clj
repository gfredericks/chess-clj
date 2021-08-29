(ns com.gfredericks.chess.proof-games.distances
  (:require
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
