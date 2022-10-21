(ns aoc2015.day01)

;part 1

(let [counts (frequencies (seq(slurp "input/day01.txt")))]
  (- (get counts \() (get counts \))))
; 280


;part 2

(loop [i 0
       floor 0
       data (map {\( 1 \) -1} (seq (slurp "input/day01.txt")))]
  (cond (= floor -1) i
        (nil? data) (str "floor " floor)
        :else (recur (inc i)
                     (+ floor (first data))
                     (next data))))
; 1797
