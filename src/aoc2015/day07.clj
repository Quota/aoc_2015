(ns aoc2015.day07
  (:require [clojure.string :refer [split-lines split]]))

(defn ast-calc
  [op arg-values]
  (case op
    :VALUE (first arg-values)
    :NOT (bit-and 65535 (bit-not (first arg-values)))
    :AND (apply bit-and arg-values)
    :OR (apply bit-or arg-values)
    :LSHIFT (apply bit-shift-left arg-values)
    :RSHIFT (apply unsigned-bit-shift-right arg-values)))

(defn ast-resolve
  ([ast-data token]
   ; add cache to ast-data (unless already there)
   ; (very important, otherwise it runs like forever...)
   (ast-resolve ast-data (atom {}) token))
  ([ast-data ast-cache token]
   (cond
     ; a literal number?
     (number? token) token
     ; everything else is a "variable"
     ; and needs to be "calculated"
     :else (if-let [cached-val (get @ast-cache token)]
             cached-val
             (let [[op & args] (get-in ast-data [:nodes token])
                   ast-val (ast-calc op (map #(ast-resolve ast-data ast-cache %) args))]
               (when (number? ast-val)
                 (swap! ast-cache assoc token ast-val))
               ast-val)))))

(defn num-or-kw
  [s]
  (let [l0 (.charAt s 0)]
    (if (Character/isDigit l0)
      (Integer/parseInt s)
      (keyword s))))

(defn ast-read-line
  "Input: \"arg0 op arg1 -> res\" (left hand side can vary)
  Output: [res [:op arg0 arg1]]"
  [nodes line]
  (let [tokens (split line #" ")
        res (num-or-kw (peek tokens))]
    (case (count tokens)
      ; 123 -> x  ==>  [x [:VALUE 123]]
      3 (assoc nodes res [:VALUE (num-or-kw (first tokens))])
      ; NOT 123 -> x  ==>  [x [:NOT 123]]
      4 (assoc nodes res [:NOT (num-or-kw (second tokens))])
      ; y (AND|OR|LSHIFT|RSHIFT) 456 -> x  ==>  [x [:OP y 456]]
      5 (assoc nodes res [(keyword (second tokens)) (num-or-kw (first tokens)) (num-or-kw (get tokens 2))]))))

(defn ast-run
  [lines & args]
  (let [ast-nodes (reduce ast-read-line {} lines)]
    (for [c args]
      (ast-resolve {:nodes ast-nodes} c))))

(defn part1 []
  (-> "input/day07.txt"
      slurp
      split-lines
      (ast-run :a)
      first))
; 956

(defn part2 []
  (-> "input/day07.txt"
      slurp
      split-lines 
      ; let `b` be the value of part-1's `a``
      (conj (str (part1) " -> b"))
      (ast-run :a)
      first))
; 40149
