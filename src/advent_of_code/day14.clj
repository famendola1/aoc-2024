(ns advent-of-code.day14
  (:require [advent-of-code.utils :as u]))

(def HEIGHT 103)
(def WIDTH 101)

(defn- predict-one-second [[[col row] robots]]
  (into {}
        (reduce (fn [new-robots [v-col v-row]]
                  (conj new-robots [[(mod (+ col v-col) WIDTH) (mod (+ row v-row) HEIGHT)]
                                    [[v-col v-row]]]))
                []
                robots)))

(defn- predict-seconds [seconds robots]
  (reduce (fn [robots-at-time s]
            (apply merge-with concat (map predict-one-second robots-at-time)))
          robots
          (range seconds)))

(defn- determine-quadrant [[col row]]
  (let [vert (quot WIDTH 2)
        horiz (quot HEIGHT 2)]
    (cond (or (= col vert) (= row horiz)) nil
          (and (< col vert) (< row horiz)) 1
          (and (> col vert) (< row horiz)) 2
          (and (< col vert) (> row horiz)) 3
          (and (> col vert) (> row horiz)) 4)))

(defn- calculate-safety-factor [robots]
  (reduce *
          (->> robots
               ((comp vals #(dissoc % nil) (partial group-by (comp determine-quadrant first))))
               (map (comp (partial reduce +) (partial map (comp count last)))))))

(defn part-1
  "Day 14 Part 1"
  [input]
  (->> input
       (u/parse-out-longs)
       (partition 4)
       (map #(into {} (vector (update (vec (map vec (partition 2 %))) 1 vector))))
       (apply merge-with concat)
       (predict-seconds 100)       
       (calculate-safety-factor)))

(defn- has-consecutive-robots? [robots]
  (let [rows (sort (map last robots))]
    ;; Arbitrarily chose 10 and it worked for my input.
    (some (comp (partial < 10) count)
          (remove (comp zero? first)
                  (partition-by zero?
                                (map #(if (= %1 (inc %2)) 1 0)
                                     (rest rows)
                                     rows))))))

(defn- find-easter-egg [robots]
  (loop [seconds 0
         robots-at-time robots]
    ;; Look for a vertical line of consecutive robots.
    (if (some has-consecutive-robots? (vals (group-by first (keys robots-at-time))))
      seconds
      (recur (inc seconds) (predict-seconds 1 robots-at-time)))))

(defn part-2
  "Day 14 Part 2"
  [input]
  (->> input
       (u/parse-out-longs)
       (partition 4)
       (map #(into {} (vector (update (vec (map vec (partition 2 %))) 1 vector))))
       (apply merge-with concat)
       (find-easter-egg)))
