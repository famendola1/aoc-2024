(ns advent-of-code.day04
  (:require [advent-of-code.utils :as u]))

(defn- as-map [matrix]
  (into {}
        (for [row (range (count matrix))
              col (range (count (first matrix)))
              :let [coord [row col]]]
          [coord (get-in matrix coord)])))

(defn- find-letter [letter word-search]
  (map #(assoc {} :coord (first %) :dir nil)
       (filter #(= (last %) letter) word-search)))

(defn- neighbors [[row col]]
  [[[(dec row) (dec col)] :up-left]
   [[(dec row) col] :up]
   [[(dec row) (inc col)] :up-right]
   [[row (dec col)] :left]
   [[row (inc col)] :right]
   [[(inc row) (dec col)] :down-left]
   [[(inc row) col] :down]
   [[(inc row) (inc col)] :down-right]])

(defn- get-next-letter [next-letter word-search {coord :coord
                                                 dir :dir}]
  (map #(assoc {} :coord (first %) :dir (last %))
       (filter #(and (or (nil? dir) (= dir (last %)))
                     (= (word-search (first %)) next-letter))
               (neighbors coord))))

(defn- find-letter-neighbors [letter-coords next-letter word-search]
  (remove nil?
          (mapcat (partial get-next-letter next-letter word-search) letter-coords)))

(defn- find-word [word word-search]
  (reduce (fn [res letter]
            (if-not res
              (reduced '())
              (find-letter-neighbors res letter word-search)))
          (find-letter (first word) word-search)
          (rest word)))

(defn part-1
  "Day 04 Part 1"
  [input]
  (->> input
       (u/to-matrix)
       (as-map)
       (find-word "XMAS")
       (count)))

(defn- is-x? [{[row col] :coord} word-search]
  (let [top-left (word-search [(dec row) (dec col)])
        top-right (word-search [(dec row) (inc col)])
        bot-left (word-search [(inc row) (dec col)])
        bot-right (word-search [(inc row) (inc col)])
        top-left-bot-right (frequencies [top-left bot-right])
        top-right-bot-left (frequencies [top-right bot-left])]
    (and (= 1 (top-left-bot-right \M))
         (= 1 (top-right-bot-left \M))
         (= 1 (top-left-bot-right \S))
         (= 1 (top-right-bot-left \S)))))

(defn- find-x [word-search]
  (filter #(is-x? % word-search) (find-letter \A word-search)))

(defn part-2
  "Day 04 Part 2"
  [input]
  (->> input
       (u/to-matrix)
       (as-map)
       (find-x)
       (count)))
