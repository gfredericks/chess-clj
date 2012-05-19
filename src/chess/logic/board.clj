(ns chess.logic.board
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        [clojure.tools.macro :only [macrolet]]
        [clojure.core.incubator :only [-?>]]))

(def ^:private sq-syms
  (for [x "abcdefgh"
        y (rest (range 9))]
    (symbol (str x y))))

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

(defne board-ranko
  [rank]
  ([[a b c d e f g h]]))


;; is letting these variables be fresh an issue with goals not halting?
(defne boardo
  [ranks]
  ([[a b c d e f g h]]
     (board-ranko a)
     (board-ranko b)
     (board-ranko c)
     (board-ranko d)
     (board-ranko e)
     (board-ranko f)
     (board-ranko g)
     (board-ranko h)))

;; Did this "backwards" so that a default pretty-print of a board
;; is oriented the way you would expect.
(defn ranks-entryo
  [board rank entry]
  (fresh [x8 x7 x6 x5 x4 x3 x2 x1]
         (== board [x8 x7 x6 x5 x4 x3 x2 x1])
         (matche [rank]
                 ([8] (== entry x8))
                 ([7] (== entry x7))
                 ([6] (== entry x6))
                 ([5] (== entry x5))
                 ([4] (== entry x4))
                 ([3] (== entry x3))
                 ([2] (== entry x2))
                 ([1] (== entry x1)))))

(defn rank-entryo
  [rank file entry]
  (fresh [a b c d e f g h]
         (== rank [a b c d e f g h])
         (matche [file]
                 ([:a] (== entry a))
                 ([:b] (== entry b))
                 ([:c] (== entry c))
                 ([:d] (== entry d))
                 ([:e] (== entry e))
                 ([:f] (== entry f))
                 ([:g] (== entry g))
                 ([:h] (== entry h)))))

(defne board-entryo
  [board square piece]
  ([board [file rank] piece]
     (boardo board)
     (fresh [rank']
            (ranks-entryo board rank rank')
            (rank-entryo rank' file piece))))

(defne rank-almost-equalo
  [rank-a rank-b file relater]
  ([[a . r] [b . r] :a _] (relater a b))
  ([[ab . ra] [ab . rb] x _]
     (fresh [x']
            (file-inco x' x)
            (rank-almost-equalo ra rb x' relater))))

(defne board-almost-equalo
  [ranks-a ranks-b rank relater]
  ([[a . r] [b . r] 8 _] (relater a b))
  ([[ab . ra] [ab . rb] x _]
     (fresh [x']
            (rank-inco x x')
            (board-almost-equalo ra rb x' relater))))

(defn board-entry-changeo
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
