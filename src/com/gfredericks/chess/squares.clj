(ns com.gfredericks.chess.squares
  "Code for handling the 64 squares on the board. Defines a public var
  for each square in algebraic notation (a1 -> h8)")

(defn square
  "Given col and row which are as returned by the col and row functions,
  returns the corresponding square."
  [^long col ^long row]
  {:pre [(<= 0 col 7) (<= 0 row 7)]}
  (-> row
      (bit-shift-left 3)
      (bit-or col)))

(defn square? [x] (and (integer? x) (<= 0 x 63)))

(def all-squares
  (for [row (range 8), col (range 8)]
    (square col row)))

(defmacro alg-notation-helper
  []
  (cons 'do
        (for [row (range 8)
              :let [row-name (inc row)]
              col (range 8)
              :let [col-name (nth "abcdefgh" col)
                    var-name (symbol (str col-name row-name))]]
          (list 'def var-name (square col row)))))

;; defines all 64 of a1 ... h8
(alg-notation-helper)

(defn col
  "Returns one of (range 8) where 0 is the leftmost column and 7 is
  the rightmost."
  [^long sq]
  (bit-and sq 7))
(defn row
  "Returns one of (range 8) where 0 is white's starting row and 7 is
  black's starting row."
  [^long sq]
  (bit-shift-right sq 3))

(defn translate-row
  "Returns nil if the resulting square is off the board. A positive delta
  moves towards black's starting row."
  [^long sq delta]
  (let [x (+ sq (* 8 delta))]
    (if (<= 0 x 63) x)))

(defn translate-col
  "Returns nil if the resulting square is off the board. A positive delta
  moves toward the kingside."
  [^long sq delta]
  (let [col (col sq)
        new-col (+ col delta)]
    (if (<= 0 new-col 7)
      (+ sq delta))))

(defn translate
  "Returns nil if the resulting square is off the board."
  [^long sq drow dcol]
  (if-let [sq' (translate-row sq drow)]
    (translate-col sq' dcol)))

(defn set-col
  "Returns a square in the same row as the given square but with the
  given column."
  [^long sq ^long col]
  (-> sq
      (bit-and 56)
      (bit-or col)))

(defn set-row
  "Returns a square in the same column as the given square but with
  the given row."
  [^long sq ^long row]
  (-> sq
      (bit-and 7)
      (bit-or (bit-shift-left row 3))))

;; Rows by role

(defn promotion-row
  [color]
  (case color :white 7 :black 0))

(defn antepromotion-row
  [color]
  (case color :white 6 :black 1))

(defn pawn-jump-row
  "The row that pawns jump to on their first move."
  [color]
  (case color :white 3 :black 4))

(defn pawn-start-row
  [color]
  (case color :white 1 :black 6))

(defn pawn-jump-over-row
  "The row that pawns jump over."
  [color]
  (case color :white 2 :black 5))
