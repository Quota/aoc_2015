(ns aoc2015.day04
  (:require [clojure.string :refer [split]])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

;both

; from: https://gist.github.com/jizhang/4325757
(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        ; raw will be of type byte[]
        raw (.digest algorithm (.getBytes s))] 
    ;(format "%032x" (BigInteger. 1 raw))
    raw
    ))


;part 1

(loop [i 0]
  (let [[d1 d2 d3] (take 3 (md5 (str "yzbqklnj" i)))]
    (cond (and (zero? d1) (zero? d2) (nat-int? d3) (< d3 16)) i
          (> i 1000000) (inc (- i)) ; don't let it run infinitely...
          :else (recur (inc i)))))
; 282749

;part 1

(loop [i 0]
  (let [[d1 d2 d3] (take 3 (md5 (str "yzbqklnj" i)))]
    (cond (and (zero? d1) (zero? d2) (zero? d3)) i
          (> i 10000000) (inc (- i)) ; don't let it run infinitely...
          :else (recur (inc i)))))
; 9962624 (circa 22 seconds)
