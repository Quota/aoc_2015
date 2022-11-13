(ns aoc2015.day14
  (:require [clojure.string :as s])
  (:require [clojure.math.combinatorics :as combo]))

; Reindeer Olympics

; introduced in clojure 1.11:
(defn update-vals
  "Calls f on every value of the given map in order to replace that value."
  [f m]
  (persistent! (reduce-kv #(assoc! %1 %2 (f %3)) (transient {}) m)))

(defn read-data
  "Input: Lines like \"\".
  Output: {:reindeer1 {:speed val :flies val :rests val} ...}
  (Speed is in km/s, flying and resting times are in seconds.)"
  [lines]
  (->> lines
       ; convert line like "Rudolph can fly 20 km/s for 3 seconds, but then must rest for 10 seconds." to ["Rudolf" "20" "3" "10"]
       (map (fn[line] (rest (re-find #"(\S+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." line))))
       (reduce (fn[trans-map [reindeer speed flies rests]]
                 (assoc! trans-map
                         (keyword (s/lower-case reindeer))
                         {:speed (Integer/parseInt speed)
                          :flies (Integer/parseInt flies)
                          :rests (Integer/parseInt rests)}))
               (transient {}))
       persistent!))

; part 1

(defn calc-distance
  [total-time {:keys [speed flies rests]}]
  ; totel-distance := speed * flies * (total-time int-div (flies + rests))
  (let [unit (+ flies rests)
        unit-count (int (/ total-time unit))
        seconds-of-last-unit (rem total-time unit)]
     (+ (* speed flies unit-count)
        (* speed (min flies seconds-of-last-unit)))))

(defn part-1 []
  (->> (slurp "input/day14.txt")
       s/split-lines
       read-data
       ; {:reindeer1 {<stats>} ...} -> {:reindeer1 <distance> ...}
       (update-vals (partial calc-distance 2503))
       ; sort by distance (descending)
       (sort-by (comp - val))))
; [:donner 2655]

; part 2


(defn get-keys-with-max-val
  "Input: {:reindeer1 <distance1> :reindeer2 <distance2> ...}
  Output: (:reindeer1 ...)"
  [m]
  (let [max-val (apply max (vals m))]
    (->> m
         (filter #(= max-val (val %)))
         keys)))
;(get-keys-with-max-val {:a 10 :b 8 :c 12 :d 7 :e 12})

(defn part-1 []
  (->> (slurp "input/day14.txt")
       s/split-lines
       read-data
       ; for ever second calc the dinstance every reindeer has travelled
       ; {:r1 {<stats>} :r2 {<stats>} ...} -> ( {:r1 <dist-at-t0> ...} {:r1 <dist-at-t1> ...} ... )
       ((fn [rein-stats]
          (for [t (range 2503)]
            (update-vals (partial calc-distance (inc t)) rein-stats))))
       ; for every t find the reindeers with have max distance
       ; -> ( #{:r1 :r2 ...} #{:r1 ...} ... )
       ; so every reindeer in this list of sets means one point for that reindeer
       ; => count the keys => points
       (map get-keys-with-max-val)
       ; finally flatten the whole structure and count the reindeers
       flatten
       frequencies
       (sort-by (comp - val))))
; [:vixen 1059]
