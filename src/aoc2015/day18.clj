(ns aoc2015.day18
  (:require [clojure.string :as s])
  (:require [clojure.math.combinatorics :as combo]))

; Like a GIF For Your Yard - Conway's Game of Life

(defn read-data
  "Input: \"#.##..<etc>\\n.###..<etc>\\n<etc>\"
  Output: {:grid [[true false true true false false ...] [false true true true false false ...] ...]
           :width width
           :height height}"
  [lines]
  (let [g (->> lines
               s/split-lines
               (map (comp (partial into []) (partial map {\. false \# true})))
               (into []))
        h (count g)
        w (count (first g))]
    {:grid g :width w :height h}))

(defn print-grid
  ([grid] (print-grid nil grid))
  ([prefix grid]
   (when prefix (println prefix))
   (->> grid
        (map (partial map {false \. true \# nil \ }))
        (map (partial apply str))
        (run! println))))

(defn get-neighbors
  [game x y]
  (for [yy (range (dec y) (+ y 2))
        xx (range (dec x) (+ x 2))]
    (if (and (= x xx) (= y yy))
      nil
      (get-in (game :grid) [yy xx] nil))))

(defn on-to-off?
  [game x y]
  (let [c (count (filter identity (get-neighbors game x y)))]
    (not (or (= c 2) (= c 3)))))

(defn off-to-on?
  [game x y]
  (= (count (filter identity (get-neighbors game x y))) 3))

(defn evolve
  [game]
    (let [grid (atom (game :grid))]
      (doseq [y (range (game :height))
              x (range (game :width))]
        (if (get-in (game :grid) [y x]) 
          ; true = on -> check if it gets turned off
          (when (on-to-off? game x y) (swap! grid update y assoc x false))
          ; false = off -> check if we need to turn it on again
          (when (off-to-on? game x y) (swap! grid update y assoc x true))))
      (assoc game :grid @grid)))


; part 1

(defn part-1 []
  (let [data (read-data (slurp "input/day18.txt"))
        gs-nth (time (nth (iterate evolve data) 100))]
    ;(print-grid (gs-nth :grid))
    (->> gs-nth
         :grid
         flatten
         (filter identity)
         count)))
; 768 (ca. 5s)

; part 2

(defn corners-on
  [game]
  (let [xmax (dec (game :width))
        ymax (dec (game :height))
        top-row (assoc (get (game :grid) 0) 
                       0 true
                       xmax true)
        bot-row (assoc (get (game :grid) ymax)
                       0 true
                       xmax true)]
    (update game :grid
            assoc 0 top-row 
                  ymax bot-row)))

(defn evolve-corner-case
  [game]
  (corners-on (evolve (corners-on game))))

(defn part-2 []
  (let [data (read-data (slurp "input/day18.txt"))
        gs-nth (time (nth (iterate evolve-corner-case data) 100))]
    (->> gs-nth
         :grid
         flatten
         (filter identity)
         count)))
; 781 (ca. 5s)
