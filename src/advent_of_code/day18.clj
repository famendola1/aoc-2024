(ns advent-of-code.day18
  (:require [advent-of-code.utils :as u]))

(def TARGET [70 70])
(def NUM_BYTES 1024)
(def is-out-of-bounds? (u/out-of-bounds (inc (first TARGET)) (inc (last TARGET))))

(defn- get-neighbors-dijkstra [bytes coord]
  (apply merge (map #(assoc {} %  1)
                    (keep #(when-not (or (is-out-of-bounds? (:coord %))
                                         (bytes (:coord %)))
                             (:coord %))
                          (u/cardinal-neighbors coord)))))

(defn part-1
  "Day 18 Part 1"
  [input]
  (let [bytes (->> input
                   (u/to-lines)
                   (take NUM_BYTES)
                   (map (comp vec reverse u/parse-out-longs))
                   (set))
        graph (u/dijkstra [0 0] TARGET (partial get-neighbors-dijkstra bytes))]
    (first (graph TARGET))))

(defn- get-neighbors-dfs [bytes coord]
  (keep #(when-not (or (is-out-of-bounds? (:coord %))
                       (bytes (:coord %)))
           (:coord %))
        (u/cardinal-neighbors coord)))

(defn- find-path [bytes start target]
  ((u/dfs start target #(get-neighbors-dfs bytes %)) target))

(defn- find-first-failing-byte [bytes]
  (loop [[byte & rem] (drop NUM_BYTES bytes)
         seen-bytes (set (take NUM_BYTES bytes))
         last-path (set nil)]
    (let [new-bytes (conj seen-bytes byte)]      
      (cond (nil? rem) nil
            (and (seq last-path) (not (last-path byte))) (recur rem new-bytes last-path)
            :else (if-let [path (find-path new-bytes [0 0] TARGET)]
                    (recur rem new-bytes path)
                    byte)))))

(defn part-2
  "Day 18 Part 2"
  [input]
  (let [bytes (->> input
                   (u/to-lines)
                   (map (comp vec reverse u/parse-out-longs)))]
    (reverse (find-first-failing-byte bytes))))
