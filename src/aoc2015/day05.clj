(ns aoc2015.day05
  (:require [clojure.string :refer [split]])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

; part 1

(defn three-vowels? [w]
  (>= (count (filter #{\a \e \i \o \u} w)) 3))

(defn double-in-a-row? [w]
  (some (fn[[l r]] (= l r)) (partition 2 1 w)))

(defn forbidden-pair? [w]
  (some #{'(\a \b) '(\c \d) '(\p \q) '(\x \y)} (partition 2 1 w)))


(let [words (split (slurp "input/day05.txt") #"\n")]
  (->> words
       (filter #(and (three-vowels? %) (double-in-a-row? %) (not (forbidden-pair? %))))
       count))
; 255


; part 2

(defn pair-twice? [w]
  (let [imax (dec (.length w))]
    (loop [i 0]
      (cond (= i imax) false
            (pos? (.indexOf w (.substring w i (+ i 2)) (+ i 2))) true
            :else (recur (inc i))))))

(defn letter-mirror? [w]
  (some (fn [[l m r]] (= l r)) (partition 3 1 w)))

(let [words (split (slurp "input/day05.txt") #"\n")]
  (->> words
       (filter #(and (pair-twice? %) (letter-mirror? %)))
       count))
; 55
