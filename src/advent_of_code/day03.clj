(ns advent-of-code.day03
  (:require [advent-of-code.utils :as u]))

(defn part-1
  "Day 03 Part 1"
  [input]
  (->> input
       (re-seq #"mul\(\d{1,3},\d{1,3}\)")
       (map u/parse-out-longs)
       (map (partial apply *))
       (reduce +)))

(defn- process-matches [matches]
  (loop [curr (first matches)
         rem (rest matches)
         enabled true
         result '()]
    (cond (nil? curr) (reverse result)
          (= curr "don't()") (recur (first rem) (rest rem) false result)
          (= curr "do()") (recur (first rem) (rest rem) true result)          
          :else (recur (first rem)
                       (rest rem)
                       enabled
                       (if enabled (cons curr result) result)))))

(defn part-2
  "Day 03 Part 2"
  [input]
  (->> input
       (re-seq #"(?:mul\(\d{1,3},\d{1,3}\)|don't\(\)|do\(\))")
       (process-matches)
       (map u/parse-out-longs)
       (map (partial apply *))
       (reduce +)))
