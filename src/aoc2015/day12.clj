(ns aoc2015.day12
  (:require [clojure.string :as s])
  (:require [clojure.data.json :as json])
  (:require [clojure.walk :as w]))

; JSAbacusFramework.io

; part 1: add up all numbers

; strategy:
; - consider everything non numerical as white space
; - split by that white-space
; - add the rest

(defn part-1 []
  (->> (s/split (slurp "input/day12.txt") #"[^0-9-]+")
       (remove empty?)
       (map #(Integer/parseInt %))
       (apply +)))
; 119433


; part 2: ignore 'red' numbers

; strategy: properly analyse/iterate the json (i.e. json structure)

(defn sum-elements
  [data]
  ; sum all elements of the given data using `apply +`
  ; before that, "map" the individual elements of the data.
  ; if it is a:
  ; - list/vec/set: replace it with calling `sum-elements` on it
  ; - map: like list/vec/set iff no value is "red", then 0
  ; - numbers: keep them
  ; - everything else: use 0
  (->> data
       (map ; transform elements of data if necessary
            (fn [x]
              ;(println (type x) x)
              (cond ; map: sum-elements of it's values
                    (map? x)
                    (if (->> x
                             vals
                             (filter #{"red"})
                             empty?)
                      (sum-elements (vals x))
                      0)
                    ; seqence: sum-element over it
                    (or (list? x) (vector? x) (set? x))
                    (sum-elements x)
                    ; single item: its fine
                    (number? x)
                    x
                    ; verything else: 0 to not affect the +
                    :else
                    0)))
       (apply +)))

(defn part-2
  []
  (-> (slurp "input/day12.txt")
      ;"[1,{\"c\":\"red\",\"b\":2},3]"
      ;"[1,2,3]"
      ;"[1,[2,3],4]"
      (json/read-str :key-fn keyword)
      sum-elements))
; 68466
