(ns chess.logic.pieces
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

(defn whiteo [p] (membero p [:K :Q :R :B :N :P]))

(defn blacko [p] (membero p [:k :q :r :b :n :p]))

(defn coloro [color piece]
  (conde ((== :white color) (whiteo piece))
         ((== :black color) (blacko piece))))

(defne kingo [p] ([:K]) ([:k]))
(defne queeno [p] ([:Q]) ([:q]))
(defne rooko [p] ([:R]) ([:r]))
(defne bishopo [p] ([:B]) ([:b]))
(defne knighto [p] ([:N]) ([:n]))
(defne pawno [p] ([:P]) ([:p]))
(defne blanko [p] ([:_]))
(defn pieceo [p] (conde ((whiteo p)) ((blacko p))))

;; could define bishopish and rookish