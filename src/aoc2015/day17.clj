(ns aoc2015.day17
  (:require [clojure.string :as s])
  (:require [clojure.math.combinatorics :as combo]))

; No Such Thing as Too Much - Eggnogg and containers

(defn read-data
  "Parses the given string linewise expecting one number per line.
  Input: \"v1\\nv2\\nv3...\"
  Output: (n1 n2 n3 ...)"
  [lines]
  (->> lines
       s/split-lines
       (map (fn[v] (Integer/parseInt v)))))


(defn filter-by-capacity
  "Input: target capacity and combinations of container sizes, i.e. list of 
  value-lists like ((v1 v2) (v1 v3) (vx ...) ...)
  Output: list of those value-lists where their sum equals the target capacity."
  [target-cap combs]
  (->> combs
       (filter #(= target-cap (apply + %)))))

(defn calc-best-comb-length
  "Returs how many items of the given container sizes list are needed to reach
  or exceed the given target capacity. This function does *not* sort the given
  list in any way. It take its items by successivly iterating via first.
  If you want the max amount of items needed then sort the container sizes
  list in ascending order before calling this function. In you want the 
  minimum amount of items then sort it descending.
  Input: target capacity and a list of container sizes like (5 5 10 15 ...).
  Output: Amount of item in the list needed to fulfill the target capacity."
  [target-cap cont-sizes]
  (loop [i 0 ; index
         c (first cont-sizes) ; current
         r (next cont-sizes) ; rest
         a 0] ; accumulator
    ;(println i c r a)
    (cond
      (nil? r) "ERR: target-cap never reached"
      (< a target-cap) (recur (inc i)
                      (first r)
                      (next r)
                      (+ a c))
      :else i)))

(comment
  (->> (slurp "input/day17.txt")
       read-data
       sort
       reverse
       (calc-best-comb-length 150)
       ))


; part 1

(defn part-1 []
  (let [target-cap 150 ; 25
        data (read-data (slurp "input/day17.txt")) ; _x.txt
        ; minimum and maximum combination lengths to fulfill target-cap:
        min-comb-length (calc-best-comb-length target-cap (reverse (sort data)))
        max-comb-length (calc-best-comb-length target-cap (sort data))
        target-combs (mapcat identity; flatten but only 1 level
                             (for [n (range min-comb-length (inc max-comb-length))]
                               (as-> data $
                                 (map-indexed (fn[x y] [x y]) $) ; need index, otherwise...
                                 (combo/combinations $ n) ; ...this line removes duplicates
                                 (map (fn[x] (map second x)) $) ; (afterwards remove index again)
                                 (filter-by-capacity target-cap $))))]
    {:min-comb-length min-comb-length
     :max-comb-length max-comb-length
     :count (count target-combs)}))
; {:min-comb-length 4, :max-comb-length 11, :count 1638}
; (no ":combs" in the result b/c there are too many...)

(comment
  ; combo/combinations removes duplicate values.
  ; so add an index before combo/combinations, and remove the index again afterwards.
  ; the following tests that idea:
  (= '((20 15) (20 10) (20 5) (20 5) (15 10) (15 5) (15 5) (10 5) (10 5) (5 5))
     (->> (slurp "input/day17_x.txt")
          read-data
          (map-indexed (fn[x y] [x y]))
          (#(combo/combinations % 2))
          (map (fn[x] (map second x)))
          ))) 

; part 2

(defn part-2 []
  (let [target-cap 150 ; 25
        data (read-data (slurp "input/day17.txt")) ; _x.txt
        ; minimum combination length to fulfill target-cap:
        min-comb-length (calc-best-comb-length target-cap (reverse (sort data)))
        target-combs (as-> data $
                          (map-indexed (fn[x y] [x y]) $) ; need index, otherwise...
                          (combo/combinations $ min-comb-length) ; ...this line removes duplicates
                          (map (fn[x] (map second x)) $) ; (afterwards remove index again)
                          (filter-by-capacity target-cap $))]
    {:min-comb-length min-comb-length
     :count (count target-combs)
     :combs target-combs }))
; {:min-comb-length 4, :count 17 :combs ...}
