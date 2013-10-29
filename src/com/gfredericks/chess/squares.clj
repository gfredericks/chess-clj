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
  [^long sq row col]
  (if-let [sq' (translate-row sq row)]
    (translate-col sq' col)))
