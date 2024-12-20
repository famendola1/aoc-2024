(ns advent-of-code.day19
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.reducers :as r]))

(defn- get-next-towels [towel-positions pattern-len [idx towel]]
  (let [next-towel-pos (+ idx (count towel))]
    (if (= next-towel-pos pattern-len)
      [[pattern-len ""]]      
      (get towel-positions next-towel-pos []))))

(defn- is-pattern-possible? [towels pattern]
  (let [towel-positions (group-by first (mapcat #(u/re-pos (re-pattern %) pattern) towels))
        pattern-len (count pattern)
        target [pattern-len ""]]
    ((u/dfs [0 ""] target (partial get-next-towels towel-positions pattern-len)) target)))

(defn part-1
  "Day 19 Part 1"
  [input]
  (let [blocks (u/to-blocks input)
        towels (set (str/split (first blocks) #", "))
        patterns (u/to-lines (last blocks))]
    (count (filter (partial is-pattern-possible? towels) patterns))))

(def count-towel-combinations
  (memoize
   (fn [towels pattern]
     (if (not (seq pattern))
       1
       (->> towels
            (keep #(when (str/starts-with? pattern %)
                     (subs pattern (count %))))
            (map (partial count-towel-combinations towels))
            (apply +))))))

(defn part-2
  "Day 19 Part 2"
  [input]
  (let [blocks (u/to-blocks input)
        towels (set (str/split (first blocks) #", "))
        patterns (u/to-lines (last blocks))]
    (->> patterns
         (map (partial count-towel-combinations towels))
         (reduce +))))
