(ns chess.logic.board
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        [clojure.tools.macro :only [macrolet]]
        [clojure.core.incubator :only [-?>]]
        [misquote.core :only [misquote]]))

(def ^:private sq-syms
  (vec
   (for [y (range 8 0 -1)
         x "abcdefgh"]
     (symbol (str x y)))))

(defmacro ^:private defboard-helper
  []
  (list 'deftype 'Board (vec sq-syms)))
(defboard-helper)

(defprotocol IUnifyWithBoard
  (unify-with-board [u v s]))

(extend-type Board
  IUnifyTerms
  (unify-terms [u v s] (unify-with-board v u s)))

(macrolet [(fooze []
                  (list* `-?> 's
                         (for [sq sq-syms]
                           `(unify-terms
                             (. ~'u ~sq)
                             (. ~'v ~sq)))))]

          (extend-protocol IUnifyWithBoard
            nil
            (unify-with-board [u v s] false)
            Board
            (unify-with-board [u v s]
              (fooze))))

(extend-type Object
  IUnifyWithBoard
  (unify-with-board [u v s] false))

(defn vecs
  [coll]
  (vec (map vec coll)))

(macrolet [(foo []
                (->> sq-syms
                     (map (partial list '. 'b))
                     (partition 8)
                     vecs))]
          (defn board->data
            "Converts a Board to a 2d vector"
            [b]
            (foo)))

(defn data->board
  "Converts a 2d vector to a board"
  [v]
  (apply ->Board (apply concat v)))

(extend-type Board
  IWalkTerm
  (walk-term [board s]
    (->> board
         board->data
         (apply concat)
         (map #(walk* s %))
         (partition 8)
         data->board))
  IReifyTerm
  (reify-term [board s]
    (->> board
         board->data
         (apply concat)
         (reduce -reify* s))))

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

(macrolet [(foo [name]
                `(fresh ~sq-syms
                        (== ~name ~(cons '->Board sq-syms))))]
          (defn boardo
            [board]
            (foo board)))

(def sq-syms-with-sqrs
  (for [sq-sym sq-syms]
    [sq-sym [(-> sq-sym name first str keyword)
             (-> sq-sym name second str read-string)]]))

(macrolet [(foo []
                (list* 'defne 'board-entryo
                       '[board square piece]
                       (for [[sq-sym sq] sq-syms-with-sqrs]
                         (list ['_ sq '_] `(== ~'piece (. ~'board ~sq-sym))))))]
          (foo))

(macrolet [(bec []
                (list 'defn 'board-entry-changeo
                      '[before-board after-board square before-entry after-entry]
                      (list* `all
                             (list 'boardo 'before-board)
                             (list 'boardo 'after-board)
                             (for [[sq-sym sq] sq-syms-with-sqrs]
                               `(conde ((== (. ~'before-board ~sq-sym)
                                            (. ~'after-board ~sq-sym)))
                                       ((== ~'square ~sq)
                                        (== (. ~'before-board ~sq-sym) ~'before-entry)
                                        (== (. ~'after-board ~sq-sym) ~'after-entry)))))))]
          (bec))

#_(defne rank-almost-equalo
  [rank-a rank-b file relater]
  ([[a . r] [b . r] :a _] (relater a b))
  ([[ab . ra] [ab . rb] x _]
     (fresh [x']
            (file-inco x' x)
            (rank-almost-equalo ra rb x' relater))))

#_(defne board-almost-equalo
  [ranks-a ranks-b rank relater]
  ([[a . r] [b . r] 8 _] (relater a b))
  ([[ab . ra] [ab . rb] x _]
     (fresh [x']
            (rank-inco x x')
            (board-almost-equalo ra rb x' relater))))

#_(defn board-entry-changeo
  "Succeeds when before-board and after-board are the same boards
  except at square"
  [before-board after-board square before-entry after-entry]
  (fresh [x y]
         (== square [x y])
         (board-almost-equalo
          before-board
          after-board
          y
          (fn [r1 r2]
            (rank-almost-equalo r1 r2 x
                                (fn [b a]
                                  (== b before-entry)
                                  (== a after-entry)))))))
