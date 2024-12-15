(ns advent-of-code.day12
  (:require [advent-of-code.utils :as u]
            [clojure.set :as set]))

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

(defn- compute-perimeter [region]
  (reduce +
          (map #(- 4 (count
                      (filter region
                              (map :coord (u/cardinal-neighbors %)))))
               region)))

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

(defn- count-vertical-neighbors [region [row col]]
  (count (filter region [[(dec row) col]
                         [(inc row) col]])))

(defn- count-vertical-sides [region row]
  (prn row)
  (reduce (fn [res [[p-row p-col :as pos] nbrs]]
            (let [prev-pos [p-row (dec p-col)]]
              (cond (nil? (row prev-pos)) (+ res (- 2 (row pos)))
                    (= (row prev-pos) (row pos)) res
                    (= (dec (row prev-pos)) (row pos)) (inc res)
                    :else res)))
          0
          row))

(defn- find-vertical-sides [region]
  (prn region)
  (let [rows (vals (group-by first region))]
    (reduce +
            (map (partial count-vertical-sides region)
                 (map (comp (partial u/dissoc-by-vals #(= 2 %))
                            (partial into (sorted-map))
                            (partial map #(vector % (count-vertical-neighbors region %))))
                      rows)))))

(defn- transpose-region [region]
  (set (map #(vector (last %) (first %)) region)))

(defn- count-sides [region]  
  (u/debug (+ (u/debug (find-vertical-sides region)) (u/debug (find-vertical-sides (transpose-region region))))))

(defn- compute-price-alt [region]
  (* (count region) (count-sides region)))

(defn part-2
  "Day 12 Part 2"
  [input]
  (->> input
       ((comp u/matrix->map u/to-matrix))
       (find-regions)
       (map compute-price-alt)
       (reduce +)))
