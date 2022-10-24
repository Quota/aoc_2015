(ns aoc2015.day06
  (:require [clojure.string :refer [split-lines replace-first]])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

; part 1

(defn exec-lights-instruction
  [lights! instr-line]
  (let [[instr x0 y0 x1 y1] (rest (re-matches #"(\D+) (\d+),(\d+) through (\d+),(\d+)" instr-line))
        reduce-fn (case instr
                    "toggle" #(if (%1 %2) (disj! %1 %2) (conj! %1 %2))
                    "turn on" #(conj! %1 %2)
                    "turn off" #(disj! %1 %2))]
    (reduce reduce-fn
            lights!
            (for [x (range (Integer/parseInt x0) (inc (Integer/parseInt x1)))
                  y (range (Integer/parseInt y0) (inc (Integer/parseInt y1)))]
              [x y]))))

(comment
  (time
    (->> (split-lines (slurp "input/day06.txt"))
         (reduce exec-lights-instruction (transient #{}))
         persistent!
         count))
  ; 400410 (ca. 33s runtime)
  )


; part 2

(defn exec-brightness-instruction
  [brightness! instr-line]
  (let [[instr x0 y0 x1 y1] (rest (re-matches #"(\D+) (\d+),(\d+) through (\d+),(\d+)" instr-line))
        reduce-fn (case instr
                    "toggle" #(assoc! %1 %2 (+ 2 (%1 %2 0)))
                    "turn on" #(assoc! %1 %2 (inc (%1 %2 0)))
                    "turn off" #(if (pos? (%1 %2 0)) (assoc! %1 %2 (dec (%1 %2))) %1))]
    (reduce reduce-fn
            brightness!
            (for [x (range (Integer/parseInt x0) (inc (Integer/parseInt x1)))
                  y (range (Integer/parseInt y0) (inc (Integer/parseInt y1)))]
              [x y]))))

(-> (transient {})
    (exec-brightness-instruction "turn on 0,0 through 1,1")
    (exec-brightness-instruction "toggle 2,2 through 2,2")
    persistent!
    vals)

(comment
  (time
    (->> (split-lines (slurp "input/day06.txt"))
         (reduce exec-brightness-instruction (transient {}))
         persistent!
         vals
         (#(reduce + %))))
  ; 15343601 (ca. 49s)
  )
