(ns aoc2015.day10
  (:require [clojure.string :as s]))

(defn expand [xs]
  (->> xs
       (partition-by identity)
       (map #(list (count %) (first %)))
       (apply concat)))

(defn part-1-2 []
  (time
    (->> "1321131112"
         (map #(- (int %) (int \0))) ; "123" -> (1 2 3)
         (iterate expand) ; call expand recursively
         (take 41) ; 51 for part2
         last
         count)))

; 40x -> 492982 (in ~1.5sec)
; 50x -> 6989950 (in ~22sec)
