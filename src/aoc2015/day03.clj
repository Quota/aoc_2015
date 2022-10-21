(ns aoc2015.day03
  (:require [clojure.string :refer [split]]))

;both

(defn deliver-present
  "delivery-log: map like {:x n :y n :visied #{[n n]...}}
  dir: one character of the following: ^ v > <"
  [delivery-log dir]
  (let [log-moved (case dir
               \^ (update delivery-log :y inc)
               \v (update delivery-log :y dec)
               \> (update delivery-log :x inc)
               \< (update delivery-log :x dec))]
    (update log-moved :visited conj [(:x log-moved) (:y log-moved)])))

(def delivery-start-log {:x 0 :y 0 :visited #{[0 0]}})


;part 1

(let [res (reduce deliver-present
                  delivery-start-log
                  (seq (slurp "input/day03.txt")))]
  (-> res
      (assoc :visit-count (count (:visited res)))
      (dissoc :visited)))
; 2081 


;part 2

; for the `apply map list (partition ...)` stuff see also:
; https://stackoverflow.com/questions/8980979/how-does-this-function-which-reverses-the-interleave-process-into-x-number-of-su
(let [[santa-dirs robo-dirs] (apply map list (partition 2 (seq (slurp "input/day03.txt"))))
      santa-deliveries (reduce deliver-present delivery-start-log santa-dirs)
      robo-deliveries (reduce deliver-present delivery-start-log robo-dirs)
      both-visited (into (:visited santa-deliveries) (:visited robo-deliveries))]
  {:santa (dissoc santa-deliveries :visited)
   :robo  (dissoc robo-deliveries :visited)
   ;:visited both-visited
   :count (count both-visited)})
; 2341
