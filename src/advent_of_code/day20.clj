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

(defn- get-neighbors [walls coord]
  (remove #(walls %)
          (map :coord (u/cardinal-neighbors coord))))

(defn- time-race [start end walls]
  (let [path ((u/dfs start end (partial get-neighbors walls)) end)]
    (into {} (map vector path (reverse (range 0 (count path)))))))

(defn- manhattan-coords [dist pred [row col :as coord]]
  (for [m-row (take (inc (* dist 2)) (iterate inc (- row dist)))
        m-col (take (inc (* dist 2)) (iterate inc (- col dist)))
        :let [coord [m-row m-col]]
        :when (and (pos? m-row)
                   (pos? m-col)
                   (pred (u/manhattan-dist coord m-coord) dist))]
    coord))

(defn- do-cheats [all-race-times no-cheat-total-time [coord race-time]]
  (map #(+ 2 (- no-cheat-total-time race-time) (all-race-times %))
       (filter #(= 2 (u/manhattan-dist coord %))
               (filter #(all-race-times %)
                       (mapcat (comp (partial map :coord) u/cardinal-neighbors)
                               (map :coord (u/cardinal-neighbors coord)))))))

(defn- find-cheats-time-saved [{:keys [start end walls boundary]}]
  (let [no-cheat (time-race start end walls)]
    (filter pos?
            (map (partial - (no-cheat start))
                 (mapcat (partial do-cheats no-cheat (no-cheat start))
                         no-cheat)))))

(defn part-1
  "Day 20 Part 1"
  [input]
  (->> input
       (parse-racetrack)
       (find-cheats-time-saved)
       (filter #(<= 100 %))
       (count)))


(defn part-2
  "Day 20 Part 2"
  [input]
  (->> input
       (parse-racetrack)))
