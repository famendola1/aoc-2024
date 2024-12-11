(ns advent-of-code.day06
  (:require [advent-of-code.utils :as u]))

(defn- as-map [matrix]
  (into {}
        (for [row (range (count matrix))
              col (range (count (first matrix)))
              :let [coord [row col]]]
          [coord (get-in matrix coord)])))

(defn- find-guard-start [lab-map]
  (ffirst (filter #(= \^ (last %)) lab-map)))

(defn- one-step [[[row col] dir]]
  (condp = dir
    :up [[(dec row) col] dir]
    :right [[row (inc col)] dir]
    :down [[(inc row) col] dir]
    :left [[row (dec col)] dir]))

(defn- turn [[pos dir]]
  (condp = dir
    :up [pos :right]
    :right [pos :down]
    :down [pos :left]
    :left [pos :up]))

(defn- out-of-bounds [row-bound col-bound]
  (fn [[row col]]
    (or (>= row row-bound) (< row 0)
        (>= col col-bound) (< col 0))))

(defn- predict-patrol [lab-matrix]
  (let [lab-map (as-map lab-matrix)
        is-out? (out-of-bounds (count lab-matrix) (count (first lab-matrix)))
        guard-start [(find-guard-start lab-map) :up]]
    (loop [guard-pos guard-start
           visited #{}]
      (let [next-pos (one-step guard-pos)]
        (cond (= \# (lab-map (first next-pos))) (recur (turn guard-pos) visited)
              (is-out? (first next-pos)) (conj visited (first guard-pos))
              :else (recur next-pos (conj visited (first guard-pos))))))))

(defn part-1
  "Day 06 Part 1"
  [input]
  (->> input
       (u/to-matrix)
       (predict-patrol)
       (count)))

(defn- predict-loop [lab-map row-bound col-bound]
  (let [is-out? (out-of-bounds row-bound col-bound)
        guard-start [(find-guard-start lab-map) :up]]
    (loop [guard-pos guard-start
           visited #{}]
      (let [next-pos (one-step guard-pos)]
        (cond
          (visited guard-pos) true
          (is-out? (first next-pos)) false
          (= \# (lab-map (first next-pos))) (recur (turn guard-pos) (conj visited guard-pos))
          :else (recur next-pos (conj visited guard-pos)))))))

(defn- creates-loop? [lab-map row-bound col-bound obstacle]
  (cond (= \# (lab-map obstacle)) false
        (= \^ (lab-map obstacle)) false
        :else (predict-loop (assoc lab-map obstacle \#) row-bound col-bound)))

(defn- detect-loops [lab-matrix]
  (let [row-bound (count lab-matrix)
        col-bound (count (first lab-matrix))
        lab-map (as-map lab-matrix)]
    (filter true?
            (apply pcalls (pmap #(partial creates-loop? lab-map row-bound col-bound %)
                                (for [row (range row-bound)
                                      col (range col-bound)]
                                  [row col]))))
    #_(filter (partial creates-loop? lab-map row-bound col-bound)            
              (for [row (range row-bound)
                    col (range col-bound)]
                [row col]))))

(defn part-2
  "Day 06 Part 2"
  [input]
  (->> input
       (u/to-matrix)
       (detect-loops)
       (count)))
