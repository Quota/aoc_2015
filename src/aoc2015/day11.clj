(ns aoc2015.day11
  (:require [clojure.string :as s]))

; passwords

(def forbidden-letters #{\i \o \l})

(defn index-of
  [sequence item]
  (loop [i 0
         sf (first sequence)
         sr (next sequence)]
    (cond
      (= item sf) i
      (nil? sr) nil
      :else (recur (inc i) (first sr) (next sr)))))

(defn special-inc-password
  "Check if `pwd` contains one of the forbidden letters and 
  increase above that immediately.
  Return: The new password or nil if no forbidden letters found."
  [pwd]
  (let [letter-idx (->> (for [l forbidden-letters] (.indexOf pwd l))
                        (filter nat-int?)
                        (apply min 9999))
        letter (nth pwd letter-idx nil)]
    (when letter
      (vec (concat (take letter-idx pwd) (list (char (inc (int letter)))) (repeat (- 7 letter-idx) \a))))))

(defn inc-password 
  ([opt pwd] (inc-password opt 7 pwd))
  ([opt idx pwd] 
   (if-let [pwd2 (if opt (special-inc-password pwd))]
     (recur opt 7 pwd2)
     (let [c-incd (inc (int (nth pwd idx)))]
       (if (> c-incd (int \z))
         (if (zero? idx)
           (assoc pwd 0 \a)
           (recur opt (dec idx) (assoc pwd idx \a)))
         (assoc pwd idx (char c-incd)))))))

(defn password-valid?
  [pwd]
  (and 
    ; must not contain i o l
    (->> pwd
         ; keep the three forbidden letters
         (filter forbidden-letters)
         ; true im empty, otherwise false
         empty?)
    ; increasing straight of at least three letters
    (->> pwd
         ; ((a b b) (b b c) (b c d) ...)
         (partition 3 1)
         ; keep increasing triplets
         (filter (fn[[c1 c2 c3]] (and (= 1 (- (int c2) (int c1)))
                                      (= 1 (- (int c3) (int c2))))))
         ; truthy if there are some, nil otherwise
         seq
         )
    ; two different, non-overlapping pairs of letters
    (->> pwd
         ; convert chars to ints: (a b b ...)
         (map int)
         ; group in pairs: ((a b) (b b) ...)
         (partition 2 1)
         ; map pairs to equal/not equal: (false true ...)
         (map (partial apply =))
         ; index the sequence: ([0 false] [1 true] ...)
         (map-indexed (comp vec list))
         ; keep only equals pairs: ([1 true] [5 true] ...)
         (filter second)
         ; extract indexes: (1 5 ...)
         (map first)
         ; true if at least one index distance is greater or equal 2
         ((fn [s] 
            (or (> (count s) 2)
                (and (> (count s) 1)
                     (> (- (second s)(first s)) 1))))))))
         

(defn part-1
  [opt data]
  (time
    (loop [pwd (inc-password opt (vec data)) ; 
           counter 0]
      (cond
        (= counter 10000000)
        nil
        (password-valid? pwd)
        {:new-password (apply str pwd)
         :increments counter}
        :else
        (do
          ;(println pwd)
          (recur (inc-password opt pwd) (inc counter)))))))

(comment

  ; tests:
  (part-1 false "abcdefgh")
  (part-1 true "abcdefgh")
  (part-1 false "ghijklmn")
  (part-1 true "ghijklmn")

  ; part 1
  (part-1 false "cqjxjnds")
  (part-1 true "cqjxjnds")

  ; part 2
  (part-1 false "cqjxxyzz")
  (part-1 true "cqjxxyzz")
  )

; Results:
; +-----------+-----------+----------------------+--------------------+
; | input     | output    | not optimized        | optimized          |
; |           |           | inc's    | time (ms) | inc's  | time (ms) |
; +-----------+-----------+----------+-----------+--------+-----------+
; | abcdefgh  | abcdffaa  |    17312 |       179 |  12021 |       416 |
; | ghijklmn  | ghjaabcc  |  7585800 |      5500 |    576 |        21 |
; | cqjxjnds  | cqjxxyzz  |   254078 |      1450 | 151737 |      4356 |
; | cqjxxyzz  | cqkaabcc  |   950510 |      5816 | 585122 |     17431 |
; +-----------+-----------+----------+-----------+--------+-----------+

