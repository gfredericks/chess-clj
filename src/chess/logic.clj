(ns chess.logic
  "Let's try this with logic programming instead."
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic [chess.logic board pieces]))

;;
;; Design
;;   I think we should maybe dictate that for the logic rules section a
;;   position only consists of turn/board/castling/en-passant. The half-move
;;   and full-move stuff shouldn't be included.
;;

(defn diag-stepo
  ([from to]
     (conde ((square-pos-diago from to))
            ((square-pos-diago to from))
            ((square-neg-diago from to))
            ((square-neg-diago to from))))
  ([from mid to]
     (conde ((square-pos-diago from mid) (square-pos-diago mid to))
            ((square-pos-diago to mid) (square-pos-diago mid from))
            ((square-neg-diago from mid) (square-neg-diago mid to))
            ((square-neg-diago to mid) (square-neg-diago mid from)))))

(defn rectilinear-stepo
  [from to]
  (conde ((square-horizo from to))
         ((square-horizo to from))
         ((square-vertico from to))
         ((square-vertico to from))))

(defn patho-with
  [step-fn]
  (fn [board from to]
    (letfn [(nontrivial-patho
              [a b c]
              (fresh [p]
                     (board-entryo board b p)
                     (blanko p)
                     (step-fn a b)
                     (conde ((step-fn b c))
                            ((fresh [d] (nontrivial-patho b d c))))))]
      (conde ((step-fn from to))
             ((fresh [mid]
                     (nontrivial-patho from mid to)))))))

(let [patho-1 (patho-with square-pos-diago)
      patho-2 (patho-with square-neg-diago)
      patho-3 (patho-with (fn [a b] (square-pos-diago b a)))
      patho-4 (patho-with (fn [a b] (square-neg-diago b a)))]
  (defn diag-patho
    "Succeeds iff from and to are not the same, and are related diagonally by
    only empty intervening squares."
    [board from to]
    (conde ((patho-1 board from to))
           ((patho-2 board from to))
           ((patho-3 board from to))
           ((patho-4 board from to)))))

(let [patho-1 (patho-with square-horizo)
      patho-2 (patho-with square-vertico)
      patho-3 (patho-with (fn [a b] (square-horizo b a)))
      patho-4 (patho-with (fn [a b] (square-vertico b a)))]
  (defn rectilinear-patho
    "Succeeds iff from and to are not the same, and are related diagonally by
    only empty intervening squares."
    [board from to]
    (conde ((patho-1 board from to))
           ((patho-2 board from to))
           ((patho-3 board from to))
           ((patho-4 board from to)))))

(defn normal-king-moveo
  [from to]
  (conde ((diag-stepo from to))
         ((rectilinear-stepo from to))))

(defn normal-queen-moveo
  [board from to]
  (conde
   ((diag-patho board from to))
   ((rectilinear-patho board from to))))

(def normal-rook-moveo rectilinear-patho)

(def normal-bishop-moveo diag-patho)

(defn normal-knight-moveo
  [from to]
  (fresh [mid]
         (conde
          ((square-horizo from mid)
           (conde ((square-pos-diago mid to))
                  ((square-neg-diago mid to))))
          ((square-horizo mid from)
           (conde ((square-pos-diago to mid))
                  ((square-neg-diago to mid))))
          ((square-vertico from mid)
           (conde ((square-pos-diago mid to))
                  ((square-neg-diago to mid))))
          ((square-vertico mid from)
           (conde ((square-pos-diago to mid))
                  ((square-neg-diago mid to)))))))

(defne pawn-starting-ranko
  [rank color]
  ([2 :white])
  ([7 :black]))

(defne pawn-promoting-ranko
  [rank color]
  ([8 :white])
  ([1 :black]))

(defn forwardo
  [color from to]
  (conde
   ((== :white color) (square-vertico from to))
   ((== :black color) (square-vertico to from))))

(defn peaceful-pawn-moveo
  [board color from to]
  (conde ((forwardo color from to))
         ((fresh [p x y z]
                 (forwardo color from x)
                 (forwardo color x to)
                 (== [y z] from)
                 (pawn-starting-ranko z color)
                 (board-entryo board x p)
                 (blanko p)))))

(defn attacking-pawn-moveo
  [color from to]
  (fresh [x]
         (forwardo color from x)
         (square-horizo x to)))

(defne opposite-coloro
  [a b]
  ([:white :black])
  ([:black :white]))


;;
;; attacks
;;

(defn- everyo
  [goal coll]
  (conde ((emptyo coll))
         ((fresh [a b]
                 (firsto coll a)
                 (resto coll b)
                 (goal a)
                 (everyo goal b)))))

(defn- concrete-boardo
  [board]
  (all
   (boardo board)
   (everyo (fn [rank] (everyo (fn [entry] (conde ((whiteo entry))
                                                 ((blacko entry))
                                                 ((blanko entry))))
                              rank))
           board)))

(defn now [] (System/currentTimeMillis))

(defn piece-attacko
  [board attacker-color square]
  (fresh [piece piece-at]
         (board-entryo board piece-at piece)
         (coloro attacker-color piece)
         (conde ((kingo piece)
                 (normal-king-moveo piece-at square))
                ((queeno piece)
                 (normal-queen-moveo board piece-at square))
                ((rooko piece)
                 (normal-rook-moveo board piece-at square))
                ((bishopo piece)
                 (normal-bishop-moveo board piece-at square))
                ((knighto piece)
                 (normal-knight-moveo piece-at square))
                ((pawno piece)
                 (attacking-pawn-moveo attacker-color piece-at square)))))

(defn safeo
  "Non relational goal -- arguments cannot contain fresh variables.

  Succeeds when the square is not under attack by any piece of the
  given color."
  [board square attacker-color]
  (all
   (concrete-boardo board)
   (conda
    ((piece-attacko board attacker-color square) fail)
    (succeed))))

;;
;; Positions and move legality
;;

;; Maybe to spike this we can ignore castling and en-passant and
;; promotions entirely

(defn simple-board-moveo
  "Succeeds if the after board is the result of moving the contents
  of the from-square to the to-square and everything else is the
  same."
  [bboard from-square aboard to-square piece]
  (fresh [mid-board foo blank]
         (blanko blank)
         (board-entry-changeo
          bboard
          mid-board
          from-square
          piece
          blank)
         (board-entry-changeo
          mid-board
          aboard
          to-square
          foo
          piece)))

;; We might want to add checks that the piece-set is legal? :/
;; It is certainly describeable in core.logic but not sure about
;; performance.
(defn legal-moveo
  [before-pos move after-pos]
  (fresh [from-square to-square promotion piece color before-board
          after-board other-color]
         (== move
             {:from from-square,
              :to to-square,
              :promotion promotion})
         (== before-pos
             {:turn color
              :board before-board})
         (== after-pos
             {:turn other-color
              :board after-board})
         (opposite-coloro color other-color)
         (board-entryo before-board from-square piece)
         (coloro color piece)

         ;; types of moves
         (let [move-or-capture (fresh [moved-onto]
                                      ;; should make sure it's not a king
                                      (board-entryo before-board to-square moved-onto)
                                      (conde ((blanko moved-onto))
                                             ((coloro other-color moved-onto))))]
           (conde
            ((kingo piece)
             (normal-king-moveo from-square to-square)
             (simple-board-moveo before-board from-square after-board to-square piece)
             move-or-capture)

            ((queeno piece)
             (normal-queen-moveo before-board from-square to-square)
             (simple-board-moveo before-board from-square after-board to-square piece)
             move-or-capture)

            ((rooko piece)
             (normal-rook-moveo before-board from-square to-square)
             (simple-board-moveo before-board from-square after-board to-square piece)
             move-or-capture)

            ((bishopo piece)
             (normal-bishop-moveo before-board from-square to-square)
             (simple-board-moveo before-board from-square after-board to-square piece)
             move-or-capture)

            ((knighto piece)
             (normal-knight-moveo from-square to-square)
             (simple-board-moveo before-board from-square after-board to-square piece)
             move-or-capture)

            ((pawno piece)
             (fresh [thing-at-to-square]
                    (conde
                     ;; pawn capture (but en passant!)
                     ((attacking-pawn-moveo color from-square to-square)
                      (coloro other-color thing-at-to-square))

                     ;; normal pawn move
                     ((blanko thing-at-to-square)
                      (peaceful-pawn-moveo before-board color from-square to-square)))
                    (board-entryo before-board to-square thing-at-to-square))
             (simple-board-moveo before-board from-square after-board to-square piece)
             move-or-capture)))

         ;; oh we should write a failing test showing that when moving backwards
         ;; it might leave a king in check. I think to fix it we'd have to add
         ;; a safeo clause for the other-color king in the before-position
         (fresh [enemy-king-at enemy-king]
                (kingo enemy-king)
                (coloro other-color enemy-king)
                (board-entryo before-board enemy-king-at enemy-king)
                (safeo before-board enemy-king-at color))

         ;; definitely do this last since it's not relational
         ;; if necessary we could add a clause that says each of
         ;; the board entries is something.
         (fresh [this-king-at this-king]
                (kingo this-king)
                (coloro color this-king)
                (board-entryo after-board this-king-at this-king)
                (safeo after-board this-king-at other-color))))

(defn data->pos
  "Given a user-friendly map of a position, returns a
  logic-optimized thinger."
  [{:keys [board turn]}]
  {:board (data->board board)
   :turn turn})

(defn pos->data
  "The other way"
  [{:keys [board turn]}]
  {:board (board->data board)
   :turn turn})