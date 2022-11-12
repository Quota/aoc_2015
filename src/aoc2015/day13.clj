(ns aoc2015.day13
  (:require [clojure.string :as s])
  (:require [clojure.math.combinatorics :as combo]))

; Knights of the Dinner Table

(defonce gain-lose-fn { "gain" identity "lose" - })

; introduced in clojure 1.11:
(defn update-vals
  "Calls f on every value of the given map in order to replace that value."
  [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn read-data
  "Input: lines like \"Alice would gain 54 happiness units by sitting next to Bob.\"
  Output: happiness map like: { guest1 { guest2 happyval, guest3 happyval, ...}, ...}"
  [data]
  (->> data
       ; convert line like:
       ; Alice would gain 54 happiness units by sitting next to Bob.
       ; to [guest1 <"gain"|"less"> happyval guest2]
       (map (fn[line] (rest (re-find #"(\S+) would (\S+) (\d+) happiness .* to (\S+)." line))))
       ; then convert it to [:guest1 :guest2 signed-happyval]
       (map (fn[[p1 gl v p2]] [(keyword p1) (keyword p2) ((gain-lose-fn gl) (Integer/parseInt v))]))
       ; finally create a map like: { :guest1 { :guest2 happyval, ... }, ... }
       (reduce (fn[m [p1 p2 v]] (assoc-in m [p1 p2] v)) {})))

(defn calc-happiness
  [happinesses list-of-pairs]
  (->> list-of-pairs
       (map (fn[pair] (+ (get-in happinesses pair)
                         (get-in happinesses (reverse pair)))))
       (apply +)))

(defn find-max-happiness
  "Input: happiness map as returned from `read-data`.
  Output: {:arrangement (guestX guestY ...) :happiness val}"
  [happinesses]
  (let [; seating arrangements are all permutations of the guests
        arrs (->> happinesses
                  keys
                  combo/permutations)]
    (->> arrs
         (map (fn[arr] 
                (let [pairs (partition 2 1 arr arr) ; 2x arr makes ring
                      happiness (calc-happiness happinesses pairs)]
                  {:arrangement (map name arr) ; keywords -> strings
                   :happiness happiness})))
         (apply max-key :happiness))))

; part 1

(defn part-1 []
  (let [happinesses (->> (slurp "input/day13.txt")
                         s/split-lines
                         read-data)]
    (find-max-happiness happinesses)))
; 709

; part 2

(defn part-1 []
  (let [happinesses (->> (slurp "input/day13.txt")
                         s/split-lines
                         read-data
                         ; add {:me 0} to happiness maps of all guests:
                         (update-vals #(assoc % :me 0))
                         ; add {:me {guest1 0, guest2 0, ...} to happinesses
                         (#(assoc % :me (zipmap (keys %) (repeat 0)))))]
    (find-max-happiness happinesses)))
; 668
