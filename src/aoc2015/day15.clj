(ns aoc2015.day15
  (:require [clojure.string :as s])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.core.matrix :as mx]))

; Science for Hungry People

(comment
  ; ingredients: teaspoons add up to 100
  100 = tsp1 + ... + tspN

  ; total capacity
  tcap = tsp1 * cap1 + ... + tspN * capN
  ; total durability
  tdur = tsp1 * dur1 + ... + tspN * durN
  ; total flavor
  tfla = tsp1 * fla1 + ... + tspN * flaN
  ; total texture
  ttex = tsp1 * tex1 + ... + tspN * texN
  ; total calories
  tcal = tsp1 * cal1 + ... + tspN * calN

  ; total score:
  score = tcap * tdur * tfla * ttex

  ; goal: highest score
)

; combinations of teaspoons for 4 ingredients which add up to 100
; (precalculated and cached -- takes 15-20 sec to create)
(def ing4-comb
  (let [start 0 end 101 total 100]
    (for [t1 (range start end)
          t2 (range start end)
          t3 (range start end)
          t4 (range start end)
          :when (= total (+ t1 t2 t3 t4))]
      [t1 t2 t3 t4])))

; combinations of teaspoons for 2 ingredients which add up to 100
(def ing2-comb
  (let [start 0 end 101 total 100]
    (for [t1 (range start end)
          t2 (range start end)
          :when (= total (+ t1 t2))]
      [t1 t2])))

(defn read-data
  "Input: Lines like \"Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5\"
  Output: {:sprinkles {:cap 5 :dur -1 :fla 0 :tex 0 :cal 5} ...}"
  [lines]
  (->> lines
       ; ("Ingridient: capacity <val>, ...", "...", ...) => (("Ingridient", "val", ...), ...)
       (map (fn[line] (rest (re-find #"(\S+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (\d+)" line))))
       (reduce (fn[m [ing cap dur fla tex cal]]
                 (assoc m 
                        (keyword (s/lower-case ing))
                        {:cap (Integer/parseInt cap)
                         :dur (Integer/parseInt dur)
                         :fla (Integer/parseInt fla)
                         :tex (Integer/parseInt tex)
                         :cal (Integer/parseInt cal)}))
               {})))

(defn make-ing-matrix
  "Input: Data like from read-data
  Output: Matrix with rows for every ingredient and columns for every 
          property (dropping calories however).
  Example: {:sprinkles {:cap 5 :dur -1 :fla 0 :tex 0 :cal 5}
            :icing {:cap 2 :dur 3 :fla -1 :tex 0 :cal 10}
            ...} 
  Becomes: [[5 -1 0 0]
            [2 3 -1 0]
            ...]"
  [ing-data]
  (->> ing-data
       vals
       (map vals)
       (map (partial take 4))
       (map vec)
       vec))

(defn make-ing-calories
  "Input: Data like from read-data
  Output: Vector with calories"
  [ing-data]
  (->> ing-data
       vals 
       (map vals)
       (map last)
       vec))

(defn calc-score
  "Input: <matrix of ingredients properties> <list of teaspoons for every ingredient>
  Output: score value
  Note that the whole score will be 0 if any of the ingredients properties becomes negative."
  [ing-matrix ts-vec]
  (let [mul-res (mx/mmul ts-vec ing-matrix)]
    (if (seq (filter neg? (mx/eseq mul-res)))
      0 ; as stated in the riddle
      (long (apply * mul-res)))))

; part 1

(defn part-1 []
  (time
    (let [ing-data (->> (slurp "input/day15.txt")
                        s/split-lines
                        read-data)
          ing-matrix (make-ing-matrix ing-data)
          ing-comb ing4-comb]
      ;ing-data
      ;ing-matrix
      ;ing-comb
      (->> ing-comb
           ; calc and add score to every teaspoon combination
           (map (fn[%] [% (calc-score ing-matrix %)]))
           ; remove zero score elements
           (remove (comp zero? second))
           ; find highest by score
           (apply max-key second)))))
; [[28 35 18 19] 13882464] (1446.6669 msecs)

; part 2

(defn calc-calories
  [ing-cals ts]
  (int (first (mx/eseq (mx/mmul ts ing-cals)))))

(defn part-1 []
  (time
    (let [ing-data (->> (slurp "input/day15.txt")
                        s/split-lines
                        read-data)
          ing-matrix (make-ing-matrix ing-data)
          ing-cals (make-ing-calories ing-data)
          ing-comb ing4-comb]
      ;ing-data
      ;ing-matrix
      ;ing-cals
      ;ing-comb
      (->> ing-comb
           ; remove all teaspoon combinations not representing 500 calories
           (filter #(= (calc-calories ing-cals %) 500))
           ; calc and add score to every teaspoon combination
           (map (fn[%] [% (calc-score ing-matrix %)]))
           ; remove zero score elements
           (remove (comp zero? second))
           ; find highest by score
           (apply max-key second)))))
; [[27 27 15 31] 11171160] (279.5009 msecs)

