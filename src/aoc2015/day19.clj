(ns aoc2015.day19
  (:require [clojure.string :as s])
  (:require [clojure.math.combinatorics :as combo]))

; Medicine for Rudolph

; introduced in clojure 1.11:
(defn update-vals
  "Calls f on every value of the given map in order to replace that value."
  [f m]
  (persistent! (reduce-kv #(assoc! %1 %2 (f %3)) (transient {}) m)))


; part 1

(defn parse-replacements-1
  "Input: (\"a => b\" \"cd => ef\" ...)
  Output: {\"a\" (\"b\" ...), \"cd\" (\"ef\" ...), ...}"
  [rs]
  (->> rs
       (map #(re-seq #"[a-zA-Z]+" %))
       (group-by first)
       (update-vals (partial map second))))
       

#_(parse-replacements-1 '("A => B" "A => Cd" "Ab => E" "Cd => A" "Cd => E" "F => Ab" "e => Cd" "e => F"))

(defn parse-molecule
  "Input: (\"\" \"\" ... \"AbCdEF...\") ; drops leading empty strings
  Output: (\"Ab\" \"Cd\" \"E\" \"F\" ...)"
  [ms]
  (->> ms
       (drop-while empty?)
       first
       (re-seq #"[A-Z][a-z]*")
       (into [])))

#_(parse-molecule ["" "AbCdEAFCd"])

(defn read-data-1
  "Input: \"a => b\ncd => ef\n...\n\nxyz...\"
  Output: {:repl {:a :b, :cd :ef, ...} :mole \"xyz...\"}"
  [lines]
  (let [[rs ms] (->> lines
                     s/split-lines
                     (split-with #(pos? (count %))))]
    {:repl (parse-replacements-1 rs)
     :mole (parse-molecule ms)}))

#_(read-data-1 (slurp "input/day19_x.txt"))

(defn part-1 []
  (let [{:keys [repl mole]} (read-data-1 (slurp "input/day19_x.txt"))
        ;mole (into [] (re-seq #"[A-Z][a-z]*" "HOHOHO"))
        len (count mole)
        all (atom (transient #{}))]
    (loop [i 0] ; i := 0 .. (count mole)
      (if (< i len)
        (do
          (doseq [r (get repl (get mole i))]
            (swap! all conj! (apply str (assoc mole i r))))
          (recur (inc i)))
        (swap! all persistent!)))
    {:count (count @all)
     :all (if (< (count @all) 10) @all :too-many)}))

; part 2

(defn parse-replacements-2
  [rs]
  (->> rs
       (map #(re-seq #"[a-zA-Z]+" %))
       (map reverse)
       flatten
       (apply hash-map)))

#_(parse-replacements-2 '("A => B" "Ab => C" "De => F" "Gh => I" "X => Y" "Y => e" ))

#_(sort-by (comp - count) (keys (:repl (read-data-2 (slurp "input/day19.txt")))))

(defn read-data-2
  "Input: \"l1 => r1\nl2 => r2\n...\n\nxyz...\"
  Output: {:repl {\"r1\" (\"l1\", ...) \"r2\" (\"l2\",...) ...} :mole \"xyz...\"}"
  [lines]
  (let [[rs ms] (->> lines
                     s/split-lines
                     (split-with #(pos? (count %))))]
    {:repl (parse-replacements-2 rs)
     :mole (second ms)}))

; does not work
(defn part-2 []
  (let [{:keys [repl mole]} (read-data-2 (slurp "input/day19.txt"))
        repl-re (re-pattern (str "(?<=.*)(" (s/join "|" (sort-by (comp - count) (keys repl))) ")"))]
    (println repl-re)
    (loop [i 0 m mole]
      (if (or (= "e" m) (>= i 32))
        i
        (do
          (println m)
          (recur (inc i) (s/replace m repl-re (fn[gs] (repl (second gs))))))))))
