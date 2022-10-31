(ns aoc2015.day09
  (:require [clojure.string :as s])
  (:require [clojure.data.priority-map :refer [priority-map]])
  (:require [clojure.math.combinatorics :as combo]))

; dijkstra algorithm functions
; (didn't need dijkstra after all...)

(defn get-path
  "Returns the path from start to end assuming `parents-map` is the parents map
   from `run-dijkstra` with start as starting node."
  [parents-map start end]
    (loop [path '(end)]
      (let [curr (peek path)
	    parent (parents-map curr)]
	(cond
	  (nil? parent) {:error (str "no parent for " curr)}
	  (not= parent start) (recur (conj path parent))
	  :else path))))

(defn dijkstra-reducer
  "Dijkstra logic (part 2).
  Input:
  - curr: current node
  - data: algorithm data so far, see `run-dijkstra`
  - nbr: neighbor of `curr` to check
  Output:
  - Map like `data`"
  [curr data nbr]  ; nbr: neighbor (of curr) to check
  (let [abs-costs ((:abs-costs data) curr)
        new-costs (+ abs-costs (get-in (:nbg-costs data) [curr nbr]))]
    ; if in queue but new-costs is better (lower),
    ; or not visited yet (no parent)?
    (if (or (and (contains? (:queue data) nbr)
                 (< new-costs abs-costs))
            (not (contains? (:parents data) nbr)))
      ; then enqueue (or update queue priority) and
      ; update value and parent
      (-> data
          (update :abs-costs assoc nbr new-costs)
          (update :parents assoc nbr curr)
          (update :queue assoc nbr new-costs))
      ; else no changes
      data)))

(defn run-dijkstra
  "Dijkstra logic (part 1).
  Input:
  - data: algorithm data as map:
    {:nbg-costs {[x y] {[n_x n_y] val, ...}, ...} ; neighbor costs from [x y] to [n_x n_y]
     :abs-costs {[x y] val, ...}  ; absolute costs so far from start to [x y]
     :parents {[x y] [p_x, p_y], ...} ; best parent [p_x p_y] so far for [x y]
     :queue {[x y] val, ...}} ; priority map, i.e. map sorted by values (not keys)
  Initially the queue should contain the start node for the algorithm to start."
  [data]
  (let [curr (first (peek (:queue data)))
        data-next (reduce (partial dijkstra-reducer curr)
                          (assoc data :queue (pop (:queue data)))
                          (keys (get (:nbg-costs data) curr)))]
    (if (empty? (:queue data-next))
      data-next
      (recur data-next))))

(defn part-1-and-2 []
  (time
    (let [data (->> "input/day09.txt"
                    slurp
                    s/split-lines
                    (map #(s/split % #" (to |= )?"))
                    (reduce (fn [m [f t c]]  ; destrcted params: from to costs
                              (let [f-kw (keyword (s/lower-case f))
                                    t-kw (keyword (s/lower-case t))
                                    c-num (Integer/parseInt c)]
                                (-> m
                                    (assoc-in [f-kw t-kw] c-num)
                                    (assoc-in [t-kw f-kw] c-num))))
                            {}))
          costs (->> data
                     keys
                     combo/permutations
                     (map (fn perm-fn [perm]
                            (->> perm
                                 (partition 2 1)
                                 (map #(get-in data %))
                                 (apply +))))
                     concat)]
      {:part1-min (apply min costs)
       :part2-max (apply max costs)})))
; 117 and 909
