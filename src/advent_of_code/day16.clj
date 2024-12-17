(ns advent-of-code.day16
  (:require [advent-of-code.utils :as u]))

(def valid-dirs {:left #{:left :up :down}
                 :up #{:up :left :right}
                 :right #{:right :up :down}
                 :down #{:down :left :right}})

(defn- get-neighbors [walls [coord dir]]
  (apply merge
         (map
          (fn [nbr]
            (if-not (= dir (:dir nbr))
              (assoc {} [coord (:dir nbr)] 1000)
              (assoc {} [(:coord nbr) dir] 1)))
          (remove (comp walls :coord)
                  (filter (comp (valid-dirs dir) :dir)
                          (u/cardinal-neighbors coord))))))

(defn part-1
  "Day 16 Part 1"
  [input]
  (let [maze (u/matrix->map (u/to-matrix input))
        walls (set (keep #(when (= \# (last %)) (first %)) maze))
        start (some #(when (= \S (last %)) (first %)) maze)
        target (some #(when (= \E (last %)) (first %)) maze)
        graph (u/dijkstra-matrix [start :right]
                                 target
                                 (partial get-neighbors walls))]
    (graph target)))

(defn- find-nodes-on-all-paths [graph start target]
  (loop [[pos & rem] (map first (filter #(= start (ffirst %)) graph))
         seen #{}]
    (cond (not (seq pos)) []
          (= (first pos) target) (conj seen pos)
          (seen pos) (recur rem seen)
          :else (recur (concat rem (seq (last (graph pos))))
                       (conj seen pos)))))
(defn part-2
  "Day 16 Part 2"
  [input]
  (let [maze (u/matrix->map (u/to-matrix input))
        walls (set (keep #(when (= \# (last %)) (first %)) maze))
        start (some #(when (= \S (last %)) (first %)) maze)
        target (some #(when (= \E (last %)) (first %)) maze)
        graph (u/dijkstra-matrix [start :right]
                                 target
                                 (partial get-neighbors walls))]
    (->> (find-nodes-on-all-paths graph target start)
         (map first)
         (set)
         (count))))
