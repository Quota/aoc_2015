(ns aoc2015.day08
  (:require [clojure.string :as s]))

(defn convert-to-clojure-str
  [string]
  (s/replace string #"(\\*)\\x" (fn [[_ grp1]] (str grp1 (if (even? (count grp1)) "\\u00" "\\x")))))

(defn part1 []
  (->> "input/day08.txt"
       slurp
       s/split-lines
       (map (fn [line] (- (count line) (count (read-string (convert-to-clojure-str line))))))
       (reduce +)))
; 1333

(defn part2 []
  (->> "input/day08.txt"
       slurp
       s/split-lines
       (map (fn [line] (- (count (pr-str line)) (count line))))
       (reduce +)))
; 2046
