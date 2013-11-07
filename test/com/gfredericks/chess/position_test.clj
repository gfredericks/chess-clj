(ns com.gfredericks.chess.position-test
  (:require [com.gfredericks.chess.position :refer :all]
            [com.gfredericks.chess.generators :as cgen]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [simple-check.clojure-test :refer [defspec]]))

(def gen-position
  "Generates a naive position."
  (gen/fmap (fn [[board turn castling en-passant full-move half-move :as tuple]]
              (zipmap [:board :turn :castling :en-passant :full-move :half-move]
                      tuple))
            (gen/tuple
             cgen/board
             (gen/elements [:white :black])
             (gen/fmap (fn [[b1 b2 b3 b4]]
                         {:white {:king b1 :queen b2}
                          :black {:king b3 :queen b4}})
                       (gen/tuple gen/boolean
                                  gen/boolean
                                  gen/boolean
                                  gen/boolean))
             cgen/square
             gen/pos-int
             gen/pos-int)))

(defspec roundtrips 1000
  (prop/for-all [pos gen-position]
    (= pos (-> pos ->fen read-fen))))
