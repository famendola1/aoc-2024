(ns advent-of-code.day10
  (:require [advent-of-code.utils :as u]))

(defn- find-trailheads [topo-map]
  (->> topo-map
       (seq)
       (keep #(when (zero? (last %)) (first %)))))

(defn- get-next-positions [pos]
  (->> pos
       (u/neighbors)
       (keep #(when (or (= :up (:dir %))
                        (= :right (:dir %))
                        (= :down (:dir %))
                        (= :left (:dir %)))
                (:coord %)))))

(defn- score-trailhead [topo-map all-trails? trailhead]
  (count
   (reduce (fn [trails pos]
             (if-not (seq trails)
               (reduced nil)
               (->> trails
                    (mapcat get-next-positions)
                    (#(if-not all-trails? (distinct %) %))
                    (filter #(= pos (topo-map %))))))
           [trailhead]
           (range 1 10))))

(defn part-1
  "Day 10 Part 1"
  [input]
  (let [topo-matrix (->> input (u/to-lines) (mapv #(mapv (comp parse-long str)
                                                         (seq %))))
        topo-map (u/matrix->map topo-matrix)]
    (->> (find-trailheads topo-map)         
         (map (partial score-trailhead topo-map false))
         (reduce +))))

(defn part-2
  "Day 10 Part 2"
  [input]
  (let [topo-matrix (->> input (u/to-lines) (mapv #(mapv (comp parse-long str)
                                                         (seq %))))
        topo-map (u/matrix->map topo-matrix)]
    (->> (find-trailheads topo-map)         
         (map (partial score-trailhead topo-map true))
         (reduce +))))
