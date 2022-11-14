(ns aoc2015.day16
  (:require [clojure.string :as s]))

; Aunt Sue

(def ticker-tape-1 {
  :children [= 3]
  :cats [= 7]
  :samoyeds [= 2]
  :pomeranians [= 3]
  :akitas [= 0]
  :vizslas [= 0]
  :goldfish [= 5]
  :trees [= 3]
  :cars [= 2]
  :perfumes [= 1]
})

(defn parse-sue-line
  "Input: \"Sue nr: attr1: val, attr2: val, ...\"
  Output: {:sue nr :attr1 val1 :attr2 val2 ...}"
  [ sue-line]
  (->> sue-line
       ; => ("Sue" "1" "cars" "9" "akitas" "3" "goldfish" "0")
       (re-seq #"\w+|\d+")
       ; => (("Sue" "1") ("cars" "9") ("akitas" "3") ("goldfish" "0"))
       (partition 2)
       ; => ((:sue 1) (:cars 9) (:akias 3) (:goldfish 0))
       (map (fn[[k v]] [(keyword (s/lower-case k)) (Integer/parseInt v)]))
       ; => {:sue 1 :cars 9 :akias 3 :goldfish 0}
       (into {})))

#_(parse-sue-line "Sue 1: cars: 9, akitas: 3, goldfish: 0")

(defn read-data
  "Input: (\"Sue 1: cars: 9, akitas: 3, goldfish: 0\", \"Sue 2: ...\")
   Output: ({:sue 1 :cars 9 :akitas 3 :goldfish:}, ...)"
  [lines]
  (->> lines
       (map parse-sue-line)))

(defn matches-ticker-tape?
  "Input: {:attr1 [op1 val1] :attr2 [op2 val2] ...}
          {:sue <nr> :attr1 val1 :attr2 val2 ...}
  Output: true/false"
  [ticker-tape sue-data]
  (reduce (fn[r [k v]]
            (let [[tt-op tt-val] (ticker-tape k)]
              (and r (tt-op v tt-val))))
          true
          (dissoc sue-data :sue)))

#_(matches-ticker-tape? ticker-tape-1 {:sue 1 :children 3 :cats 7 :trees 3})


; part 1

(defn part-1 []
  (let [data (->> (slurp "input/day16.txt")
                  s/split-lines
                  read-data)]
    (filter (partial matches-ticker-tape? ticker-tape-1) data)))
; ({:sue 373, :pomeranians 3, :perfumes 1, :vizslas 0})


; part 2

(def ticker-tape-2 
  (-> ticker-tape-1
      (assoc :cats [> 7])
      (assoc :pomeranians [< 3])
      (assoc :goldfish [< 5])
      (assoc :trees [> 3])))

(defn part-2 []
  (let [data (->> (slurp "input/day16.txt")
                  s/split-lines
                  read-data)]
    (filter (partial matches-ticker-tape? ticker-tape-2) data)))
; ({:sue 260, :goldfish 0, :vizslas 0, :samoyeds 2})
