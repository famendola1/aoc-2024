(ns advent-of-code.day02
  (:require [advent-of-code.utils :as u]))

(defn- safe-distance? [[a b]]
  (let [dist (abs (- a b))]
    (and (< dist 4) (> dist 0))))

(defn- all-increasing [num-pairs]
  (every? (partial apply <) num-pairs))

(defn- all-decreasing [num-pairs]
  (every? (partial apply >) num-pairs))

(defn- is-safe? [nums]
  (let [num-pairs (partition 2 1 nums)]
    (and (every? safe-distance? num-pairs)
         (or (all-increasing num-pairs)
             (all-decreasing num-pairs)))))

(defn part-1
  "Day 02 Part 1"
  [input]
  (->> input
       (u/to-lines)
       (map u/parse-out-longs)
       (filter is-safe?)
       (count)))

(defn- gen-alternates [report]
  (map #(u/drop-nth % report) (range 0 (count report))))

(defn part-2
  "Day 02 Part 2"
  [input]
  (->> input
       (u/to-lines)
       (map u/parse-out-longs)
       (filter #(or (is-safe? %)
                    (some is-safe? (gen-alternates %))))
       (count)))
