(ns advent-of-code.day15
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn- make-step-fn [move]
  (fn [[row col]]
    (condp = move
      \> [row (inc col)]
      \v [(inc row) col]
      \< [row (dec col)]
      \^ [(dec row) col])))

(defn- push-block [{:keys [blocks walls] :as warehouse} next-pos step-fn]
  (let [blocks-to-push (take-while blocks (iterate step-fn next-pos))
        after-blocks (step-fn (last blocks-to-push))]
    (if (walls after-blocks)
      warehouse
      (assoc warehouse
             :robot-pos next-pos
             :blocks (disj (conj blocks after-blocks) next-pos)))))

(defn- move-robot-once [{:keys [blocks walls robot-pos] :as warehouse} move]
  (let [next-pos ((make-step-fn move) robot-pos)]
    (cond (walls next-pos) warehouse
          (blocks next-pos) (push-block warehouse next-pos (make-step-fn move))
          :else (assoc warehouse :robot-pos next-pos))))

(defn- move-robot [{:keys [blocks] :as warehouse} [move & rem]]
  (if (nil? move)
    blocks
    (recur (move-robot-once warehouse move) rem)))

(defn- parse-warehouse [warehouse-str]
  (let [warehouse-matrix (u/to-matrix warehouse-str)
        warehouse-map (u/matrix->map warehouse-matrix)]
    {:width (count (first warehouse-matrix))
     :height (count warehouse-matrix)
     :robot-pos (ffirst (filter #(= \@ (last %)) warehouse-map))
     :blocks (set (keep #(when (= \O (last %)) (first %)) warehouse-map))
     :walls (set (keep #(when (= \# (last %)) (first %)) warehouse-map))}))

(defn part-1
  "Day 15 Part 1"
  [input]
  (let [block-input (u/to-blocks input)
        warehouse (parse-warehouse (first block-input))
        moves (apply str (map str/trim (u/to-lines (last block-input))))]
    (->> (move-robot warehouse moves)
         (map #(+ (* 100 (first %)) (last %)))
         (reduce +))))

(defn- expand-warehouse [[rows cols] warehouse]
  (let [walls (for [row (range rows)
                    col (range cols)
                    :when (or (= 0 row) (= 0 col) (= rows row) (= cols col))]
                [[row col] \#])
        warehouse-walls (into {} walls)]
    (reduce-kv (fn [m [row col] v]
                 (let [left [row (+ col (dec col))]
                       right [row (* 2 col)]]
                   (condp = v
                     \. (assoc m left v right v)
                     \@ (assoc m left v right \.)
                     \O (assoc m left \[ right \])
                     \# m)))
               warehouse-walls
               warehouse)))

(defn- expand-warehouse [{:keys [blocks walls [row col] width height]}]
  {:height height
   :width (* width 2)
   :robot-pos [row (* 2 col)]
   :blocks (set (map #(vector [row (*2 col)]
                              [row (inc (*2 col))])
                     blocks))
   :walls (set (mapcat #(vector [row (*2 col)]
                                [row (inc (*2 col))])
                       walls))})

(defn part-2
  "Day 15 Part 2"
  [input]
  (let [block-input (u/to-blocks input)
        warehouse (parse-warehouse (first block-input))
        moves (apply str (map str/trim (u/to-lines (last block-input))))]
    (->> (move-robot (expand-warehouse warehouse) moves)
         (map first)
         (map #(+ (* 100 (first %)) (last %)))
         (reduce +))))
