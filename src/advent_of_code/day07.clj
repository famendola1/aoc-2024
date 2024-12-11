(ns advent-of-code.day07
  (:require [advent-of-code.utils :as u]))

(defn- combinations [n choices]
  (lazy-seq
   (if (= n 0)
     [[]]
     (for [choice choices
           combo (combinations (dec n) choices)]
       (cons choice combo)))))

(defn- calculate [test numbers ops]
  (first (reduce (fn [[a b & rem] op]
                   (if (> a test)
                     (reduced '())
                     (cons (op a b) rem)))
                 numbers
                 ops)))

(defn- calibrated? [ops [test & parts]]
  (some #(= test (calculate test parts %))
        (combinations (dec (count parts)) ops)))

(defn part-1
  "Day 07 Part 1"
  [input]
  (->> input
       (u/to-lines)
       (map u/parse-out-longs)
       (filter (partial calibrated? [* +]))
       (map first)
       (reduce +)))

(defn- || [a b]
  (BigInteger. (str a b)))

(defn part-2
  "Day 07 Part 2"
  [input]
  (->> input
       (u/to-lines)
       (map u/parse-out-longs)
       (filter (partial calibrated? [* + ||]))
       (map first)
       (reduce +)))
