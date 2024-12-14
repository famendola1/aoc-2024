(ns advent-of-code.day12
  (:require [advent-of-code.utils :as u]
            [clojure.set :as set]))

(defn- parse-line [idx line]
  (let [sections (partition-by identity line)]
    (apply merge-with
           merge
           (u/debug (map #(hash-map (first %1) (hash-map [idx (first %2)] (last %2)))
                         (u/debug sections)
                         (reductions (fn [[idx len] s] (vector (+ len idx) (count s)))
                                     [0 (count (first sections))]
                                     (rest sections)))))))

(defn- parse-gardens [lines]
  (apply merge-with merge (u/debug (map-indexed parse-line lines))))

(defn- compute-areas [gardens]
  (reduce-kv (fn [m k v]
               (assoc m k (reduce + (map last v))))
             {}
             gardens))

(defn- count-borders [parts [[row col] len]]
  (count
   (filter
    (set (mapcat (fn [[[r c] l]] (map (partial vector r)
                                      (range c (+ c l))))
                 parts))
    (mapcat #(vector [(dec row) %] [(inc row) %]) (range col (+ col len))))))

(defn- compute-perimeter [garden]
  (reduce (fn [sum [pos len]]
            (- (+ sum (+ (* len 2) 2))
               (count-borders (dissoc garden pos) [pos len])))
          0
          garden))

(defn- compute-perimeters [gardens]
  (reduce-kv (fn [m k v]
               (assoc m k (compute-perimeter v)))
             {}
             gardens))

(defn- find-region [garden start seen]
  (loop [[p & rem-p] [start]
         res #{}]
    (cond (not (seq p)) res
          (or (seen p) (res p) (not= (garden start) (garden p)))
          (recur rem-p res)
          :else (recur (concat rem-p (map :coord (u/cardinal-neighbors p)))
                       (conj res p)))))

(defn- find-regions [garden]
  (loop [[p & rem-p] (mapv first garden)
         regions []
         seen #{}]
    (cond (not (seq p)) regions
          (seen p) (recur rem-p regions seen)
          :else
          (let [region (find-region garden p seen)]
            (recur rem-p (conj regions region) (set/union region seen))))))

(defn- count-borders [region]
  (reduce (fn [sum pos]
            (+ sum (count
                    (filter #(region (:coord %))
                            (u/cardinal-neighbors pos)))))
          0
          region))

(defn- compute-perimeter [region]
  (- (* (count region) 4)
     (count-borders region)))

(defn- compute-price [region]
  (* (count region) (compute-perimeter region)))

(defn part-1
  "Day 12 Part 1"
  [input]
  (->> input
       ((comp u/matrix->map u/to-matrix))
       (find-regions)
       (map compute-price)
       (reduce +)))

(defn part-2
  "Day 12 Part 2"
  [input]
  )
