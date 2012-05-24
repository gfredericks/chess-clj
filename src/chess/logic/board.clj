(ns chess.logic.board
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

;; squares (maybe we should do these with binary coordinates instead...)

(def ^:private ranks (range 1 9))
(defn ranko [x] (membero x ranks))

(defrel rank-inco ^:index a ^:index b)
(doseq [[a b] (partition 2 1 ranks)] (fact rank-inco a b))

(def ^:private files [:a :b :c :d :e :f :g :h])
(defn fileo [x] (membero x files))
(defrel file-inco ^:index a ^:index b)
(doseq [[a b] (partition 2 1 files)] (fact file-inco a b))

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

(defrel coordo ^:index pair-coord ^:index tree-coord)
(let [all-pair-coords (for [r (reverse ranks) f files] [f r]),
      bit [0 1]
      all-tree-coords (for [x1 bit
                            x2 bit
                            x3 bit
                            x4 bit
                            x5 bit
                            x6 bit]
                        [x1 x2 x3 x4 x5 x6])]
  (doseq [[pair-coord tree-coord] (map vector all-pair-coords all-tree-coords)]
    (fact coordo pair-coord tree-coord)))

;;
;; board things. Boards are binary trees with 64 leaves.
;;

(defn level-treeo
  [tree depth-in-unary goalo]
  (conde ((emptyo depth-in-unary) (goalo tree))
         ((fresh [lhs rhs dec]
                 (resto depth-in-unary dec)
                 (== [lhs rhs] tree)
                 (level-treeo lhs dec goalo)
                 (level-treeo rhs dec goalo)))))

(defn boardo
  [board goalo]
  (level-treeo board (repeat 6 0) goalo))

(let [tree-8 #(->> % (partition 4) (map (partial partition 2)))]
  (defn matrix->tree
    [m]
    (tree-8 (map tree-8 m))))

(let [pac (partial apply concat)
      tree-8-inv (comp pac pac)]
  (defn tree->matrix
    [m]
    (map tree-8-inv (tree-8-inv m))))

(defn tree-entryo
  [tree coord thing]
  (conde
   ((emptyo coord) (== tree thing))
   ((fresh [l r c cs]
           (firsto coord c)
           (resto coord cs)
           (== [l r] tree)
           (conde
            ((== 0 c) (tree-entryo l cs thing))
            ((== 1 c) (tree-entryo r cs thing)))))))

(defn board-entryo
  [board square piece]
  (fresh [tree-coord]
         (coordo square tree-coord)
         (tree-entryo board tree-coord piece)))

(defn tree-almost-equalo
  "Succeeds when two trees differ only at the named coordinate."
  [tree-a tree-b coord thing-a thing-b]
  (conde
   ((emptyo coord)
    (== tree-a thing-a)
    (== tree-b thing-b))
   ((fresh [la ra lb rb c cs]
           (firsto coord c)
           (resto coord cs)
           (== [la ra] tree-a)
           (== [lb rb] tree-b)
           (conde
            ((== 0 c)
             (tree-almost-equalo la lb cs thing-a thing-b)
             (== ra rb))
            ((== 1 c)
             (tree-almost-equalo ra rb cs thing-a thing-b)
             (== la lb)))))))

(defn board-entry-changeo
  "Succeeds when before-board and after-board are the same boards
  except at square"
  [before-board after-board square before-entry after-entry]
  (fresh [tree-coord]
         (coordo square tree-coord)
         (tree-almost-equalo before-board
                             after-board
                             tree-coord
                             before-entry
                             after-entry)))
