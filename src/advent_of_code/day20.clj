(ns advent-of-code.day20
  (:require [advent-of-code.utils :as u]
            [clojure.set :as set]))

(defn- parse-racetrack [input]
  (let [matrix (u/to-matrix input)
        as-map (u/matrix->map matrix)
        height (count matrix)
        width (count (first matrix))]
    {:start (first (keep #(when (= \S (last %)) (first %)) as-map))
     :end (first (keep #(when (= \E (last %)) (first %)) as-map))
     :walls (set (keep #(when (= \# (last %)) (first %)) as-map))
     :boundary (set (for [row (range height)
                          col (range width)
                          :when (or (zero? row) (zero? col)
                                    (= height (dec row)) (= width (dec col)))]
                      [row col]))}))

(defn- time-race [start end walls]
  (let [path ((u/dfs start end (partial get-neighbors walls)) end)]
    (zipmap path (reverse (range (count path))))))

(defn- manhattan-coords [pred dist [row col :as coord]]
  (for [m-row (take (inc (* dist 2)) (iterate inc (- row dist)))
        m-col (take (inc (* dist 2)) (iterate inc (- col dist)))
        :let [m-coord [m-row m-col]
              m-dist (u/manhattan-dist coord m-coord)]
        :when (and (pos? m-row)
                   (pos? m-col)
                   (pred m-dist dist))]
    [m-coord m-dist]))

(defn- time-saved-from-cheats [pred cheat-time all-race-times [coord race-time]]
  (map #(- race-time (last %) (all-race-times (first %)))                
       (filter #(all-race-times (first %))
               (manhattan-coords pred cheat-time coord))))

(defn- find-cheats-time-saved [pred cheat-time {:keys [start end walls boundary]}]
  (let [no-cheat-race-times (time-race start end walls)]
    (filter pos?            
            (mapcat (partial time-saved-from-cheats
                             pred cheat-time no-cheat-race-times)
                    no-cheat-race-times))))

(defn part-1
  "Day 20 Part 1"
  [input]
  (->> input
       (parse-racetrack)
       (find-cheats-time-saved = 2)
       (filter #(<= 100 %))
       (count)))


(defn part-2
  "Day 20 Part 2"
  [input]
  (->> input
       (parse-racetrack)
       (find-cheats-time-saved <= 20)
       (filter #(<= 100 %))
       (count)))
