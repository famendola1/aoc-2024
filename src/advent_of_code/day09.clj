(ns advent-of-code.day09
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

()

(defn- find-runs [disk-map]
  (->> (conj (vec disk-map) 0) ;; For alignment
       (map #(hash-map :id (quot %1 2) :idx %2 :len %3)
            (range)
            (reductions + 0 disk-map))
       (partition 2)
       (apply map vector)
       (zipmap [:files :spaces])))

(defn- move-blocks [{:keys [files spaces]}]
  (loop [files files
         spaces spaces
         res []]
    (cond
      (or (empty? spaces)
          (> (:idx (first spaces)) (:idx (peek files)))) (concat res files)
      (zero? (:len (first spaces))) (recur files (rest spaces) res)
      (zero? (:len (peek files))) (recur (pop files) spaces res)
      :else
      ;; Move blocks
      (let [{s-id :id s-len :len s-idx :idx} (first spaces)
            {f-id :id f-len :len f-idx :idx} (peek files)
            to-consume (min s-len f-len)]
        (recur
         ;; Take blocks from end and put remainder back
         (conj (pop files) {:id f-id :len (- f-len to-consume) :idx f-idx})
         ;; Consume spaces from front and put remainder after consumed spaces
         (cons {:id s-id :len (- s-len to-consume) :idx (+ s-idx to-consume)}
               (rest spaces))
         ;; Add moved blocks to results
         (conj res {:id f-id :len to-consume :idx s-idx}))))))

(defn- run-sum [{:keys [id len idx]}]
  (* id (/ (* len (+ idx (dec (+ idx len)))) 2)))

(defn part-1
  "Day 09 Part 1"
  [input]
  (->> input
       (str/trim)
       (seq)
       (map (comp parse-long str))
       (find-runs)
       (move-blocks)
       (map run-sum)
       (reduce +)))

(defn- move-files [{:keys [files spaces]}]
  (loop [files files
         spaces spaces
         res []]
    (cond
      (or (empty? spaces)
          (empty? files)) (concat res files)
      (zero? (:len (first spaces))) (recur files (rest spaces) res)
      (zero? (:len (peek files))) (recur (pop files) spaces res)
      :else
      ;; Move files
      (let [{f-id :id f-len :len f-idx :idx} (peek files)
            space (first (filter #(and (>= (:len %) f-len)
                                       (< (:idx %) f-idx))
                                 spaces))]
        (if-not (seq space)
          ;; File does not fit in space, skip file
          (recur (pop files)
                 (remove #(> (:idx %) f-idx) spaces)
                 (conj res (peek files)))
          ;; File fits in space, move file
          (recur (pop files)
                 (cons {:id (:id space) :len (- (:len space) f-len) :idx (+ (:idx space) f-len)}
                       (remove #(or (= (:id space) (:id %))
                                    (> (:idx %) f-idx)) spaces))
                 (conj res {:id f-id :len f-len :idx (:idx space)})))))))

(defn part-2
  "Day 09 Part 2"
  [input]
  (->> input
       (str/trim)
       (seq)
       (map (comp parse-long str))
       (find-runs)
       (move-files)
       (map run-sum)
       (reduce +)))
