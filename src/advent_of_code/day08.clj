(ns advent-of-code.day08
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :as comb]))

(defn- find-frequencies [row-num line]
  (let [freqs (u/re-pos #"[a-zA-Z0-9]" line)]
    (map #(vector [row-num (first %)] (last %)) freqs)))

(defn- group-frequencies [freqs [pos freq]]  
  (update freqs freq (partial cons pos)))

(defn- extrapolate-once [op dy dx [row col]]
  [(op row dy) (op col dx)])

(defn- find-antinodes [[[row-a col-a] [row-b col-b]]]
  (let [dy (- row-b row-a)
        dx (- col-b col-a)]
    [(extrapolate-once - dy dx [row-a col-a])
     (extrapolate-once + dy dx [row-b col-b])]))

(defn part-1
  "Day 08 Part 1"
  [input]
  (let [lines (u/to-lines input)
        is-out-of-bounds? (u/out-of-bounds (count lines) (count (first lines)))]
    (->> lines     
         (map-indexed find-frequencies)
         (mapcat identity)
         (reduce group-frequencies {})       
         (vals)
         (mapcat #(comb/combinations % 2))
         (mapcat find-antinodes)
         (remove is-out-of-bounds?)
         (distinct)
         (count))))

(defn- find-all-antinodes [is-out-of-bounds? [[row-a col-a] [row-b col-b]]]
  (let [dy (- row-b row-a)
        dx (- col-b col-a)]
    (concat (take-while #(not (is-out-of-bounds? %))
                        (iterate (partial extrapolate-once - dy dx)
                                 [row-a col-a]))
            (take-while #(not (is-out-of-bounds? %))
                        (iterate (partial extrapolate-once + dy dx)
                                 [row-b col-b])))))

(defn part-2
  "Day 08 Part 2"
  [input]
  (let [lines (u/to-lines input)
        is-out-of-bounds? (u/out-of-bounds (count lines) (count (first lines)))]
    (->> lines     
         (map-indexed find-frequencies)
         (mapcat identity)
         (reduce group-frequencies {})
         (vals)
         (mapcat #(comb/combinations % 2))
         (mapcat (partial find-all-antinodes is-out-of-bounds?))
         (distinct)
         (count))))
