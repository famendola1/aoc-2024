(ns advent-of-code.day01
  (:require [advent-of-code.utils :as u]))

(defn part-1
  "Day 01 Part 1"
  [input]
  (->> input
       (u/to-lines)
       (map u/parse-out-longs)
       (u/transpose)
       (map #(sort < %))
       (apply map #(abs (- %1 %2)))
       (apply +)))

(defn part-2
  "Day 01 Part 2"
  [input]
  input
  (let [[group1 group2] (->> input
                             (u/to-lines)
                             (map u/parse-out-longs)
                             (u/transpose))
        freq (frequencies group2)]
    (reduce + (map #(* %1 (or (freq %1) 0))                     
                   group1))))
