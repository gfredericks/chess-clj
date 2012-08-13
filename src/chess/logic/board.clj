(ns chess.logic.board
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))


(def ^:private ranks (range 1 9))
(defn ranko [x] (membero x ranks))

(defrel rank-inco ^:index a ^:index b)
(doseq [[a b] (partition 2 1 ranks)] (fact rank-inco a b))

(def ^:private files [:a :b :c :d :e :f :g :h])
(defn fileo [x] (membero x files))
(defrel file-inco ^:index a ^:index b)
(doseq [[a b] (partition 2 1 files)] (fact file-inco a b))

(def all-squares (set (for [rank ranks file files] [file rank])))
(defrel squareo ^:index sq)
(doseq [sq all-squares]
  (fact squareo sq))

(defne square-horizo
  [sq1 sq2]
  ([[a y] [b y]] (file-inco a b)))

(defne square-vertico
  [sq1 sq2]
  ([[x a] [x b]] (rank-inco a b)))

(defne square-pos-diago
  [sq1 sq2]
  ([[x y] [a b]] (rank-inco y b) (file-inco x a)))

(defne square-neg-diago
  [sq1 sq2]
  ([[x y] [a b]] (rank-inco b y) (file-inco x a)))

(defne board-ranko
  [rank]
  ([[a b c d e f g h]]))

;; non-relational
(let [make-board (fn [] (zipmap all-squares (repeatedly lvar)))]
  (defn boardo
    [board]
    (project [board]
             (if (lvar? board)
               (== board (make-board))
               ;; could probably just remove this for efficiency
               (if (and (map? board)
                        (-> board keys set (= all-squares)))
                 succeed
                 fail)))))

(defn board-entryo
  [board square piece]
  (all
   (squareo square)
   (project [board square]
            (== piece (board square)))))

(defn board-entry-changeo
  "Succeeds when before-board and after-board are the same boards
  except at square"
  [before-board after-board square]
  (all
   (squareo square)
   (project [before-board after-board square]
            (let [sqs (disj all-squares square)]
              (reduce composeg
                      (for [sq sqs]
                        (== (before-board sq) (after-board sq))))))))
