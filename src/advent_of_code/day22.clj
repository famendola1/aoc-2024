(ns advent-of-code.day22
  (:require [advent-of-code.utils :as u]))

(defn- evolve-once [secret]
  (let [step-1 (mod (bit-xor (* secret 64) secret) 16777216)
        step-2 (mod (bit-xor (quot step-1 32) step-1) 16777216)]
    (mod (bit-xor (* step-2 2048) step-2) 16777216)))

(defn- evolve [times secret]
  (reduce (fn [s _] (evolve-once s)) secret (range times)))

(defn part-1
  "Day 22 Part 1"
  [input]
  (->> input
       (u/parse-out-longs)
       (map (partial evolve 2000))
       (apply +)))

(defn part-2
  "Day 22 Part 2"
  [input]
  "Implement this part")
