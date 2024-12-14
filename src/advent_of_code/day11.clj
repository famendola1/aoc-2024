(ns advent-of-code.day11
  (:require [advent-of-code.utils :as u]))

(defn- split-stone [digits]
  (map (comp parse-long (partial apply str))
       (split-at (/ (count digits) 2) digits)))

(defn- apply-rules [[stone num]]
  (let [digits (str stone)]
    (cond (zero? stone) {1 num}
          (even? (count digits))
          (apply merge-with + (map #(hash-map % num) (split-stone digits)))
          :else {(* 2024 stone) num})))

(defn- blink-once [stones]
  (apply merge-with + (map apply-rules stones)))

(defn- blink [times stones]
  (nth (iterate blink-once (frequencies stones)) times))

(defn part-1
  "Day 11 Part 1"
  [input]
  (->> input
       (u/parse-out-longs)
       (blink 25)
       (vals)
       (reduce +)))

(defn part-2
  "Day 11 Part 2"
  [input]
  (->> input
       (u/parse-out-longs)
       (blink 75)
       (vals)
       (reduce +)))
