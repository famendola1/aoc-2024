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

(defn- move-robot-once [{:keys [blocks walls robot-pos] :as warehouse} move push-block-fn]
  (let [next-pos ((make-step-fn move) robot-pos)]
    (cond (walls next-pos) warehouse
          (blocks next-pos) (push-block-fn warehouse next-pos (make-step-fn move))
          :else (assoc warehouse :robot-pos next-pos))))

(defn- move-robot [{:keys [blocks] :as warehouse} [move & rem] push-block-fn]
  (if (nil? move)
    blocks
    (recur (move-robot-once warehouse move push-block-fn) rem push-block-fn)))

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
    (->> (move-robot warehouse moves push-block)
         (map #(+ (* 100 (first %)) (last %)))
         (reduce +))))

(defn- find-blocks-to-push [blocks start-pos step-fn]
  (loop [[q & rem] [start-pos]
         seen #{}
         res []]
    (cond (not (seq q)) res
          (or (seen q) (not (seq (blocks q)))) (recur rem seen res)
          :else (recur (concat rem [(step-fn q) (step-fn (blocks q))])
                       (-> seen (conj q) (conj (blocks q)))
                       (conj res [q (blocks q)])))))

(defn- push-all-big-blocks [blocks blocks-to-push step-fn]
  (reduce (fn [res [block-a block-b]]
            (let [new-a (step-fn block-a)
                  new-b (step-fn block-b)]
              (assoc res new-a new-b new-b new-a)))
          (apply dissoc blocks (mapcat identity blocks-to-push))
          blocks-to-push))

(defn- push-big-block [{:keys [blocks walls] :as warehouse} next-pos step-fn]
  (let [blocks-to-push (find-blocks-to-push blocks next-pos step-fn)]
    (if (every? (complement walls) (map step-fn (mapcat identity blocks-to-push)))
      (assoc warehouse
             :robot-pos next-pos
             :blocks (push-all-big-blocks blocks blocks-to-push step-fn))
      warehouse)))

(defn- expand-block [[row col]]
  (assoc {}
         [row (* 2 col)] [row (inc (* 2 col))]
         [row (inc (* 2 col))] [row (* 2 col)]))

(defn- expand-warehouse [{[row col] :robot-pos :keys [blocks walls width height]}]
  {:height height
   :width (* width 2)
   :robot-pos [row (* 2 col)]
   :blocks (apply merge (map expand-block blocks))
   :walls (set (mapcat #(vector [(first %) (* 2 (last %))]
                                [(first %) (inc (* 2 (last %)))])
                       walls))})

(defn part-2
  "Day 15 Part 2"
  [input]
  (let [block-input (u/to-blocks input)
        warehouse (parse-warehouse (first block-input))
        moves (apply str (map str/trim (u/to-lines (last block-input))))]
    (->> (move-robot (expand-warehouse warehouse) moves push-big-block)
         (map (partial sort-by last))
         (distinct)
         (map first)
         (map #(+ (* 100 (first %)) (last %)))
         (reduce +))))
