(ns aoc2015.day02
  (:require [clojure.string :refer [split]]))

;part 1


(reduce (fn [wrapping-paper dim]
          (let [[l w h] (map #(Integer/parseInt %) (split dim #"x"))
                lw (* l w)
                lh (* l h)
                wh (* w h)]
            (+ wrapping-paper
               (* (+ lw lh wh) 2)
               (min lw lh wh))))
        0
        (split (slurp "input/day02.txt") #"\n"))
; 1588178

;part 2

(reduce (fn [ribbon dim]
          (let [[l w h] (map #(Integer/parseInt %) (split dim #"x"))]
            (+ ribbon
               (* (- (+ l w h) (max l w h)) 2)
               (* l w h))))
        0
        (split (slurp "input/day02.txt") #"\n"))
; 3783758
