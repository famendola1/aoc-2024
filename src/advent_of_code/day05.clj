(ns advent-of-code.day05
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn- process-rules [rules]
  (reduce (fn [result curr]
            (let [[a b] (u/parse-out-longs curr)]
              (assoc result b (set (cons a (result b))))))
          {}
          rules))

(defn- is-page-updates-valid? [deps page-updates]
  (loop [good #{} bad #{} curr (first page-updates) rem (rest page-updates)]
    (cond (bad curr) false
          (not (seq rem)) true
          :else (recur (conj good curr)
                       (reduce conj bad (remove good (deps curr)))
                       (first rem)
                       (rest rem)))))

(defn- get-valid-page-updates [[rules page-updates]]
  (let [deps (process-rules rules)
        updates (map u/parse-out-longs page-updates)]
    (filter (partial is-page-updates-valid? deps) updates)))

(defn- get-middle [page-updates]
  (nth page-updates (/ (count page-updates) 2)))

(defn part-1
  "Day 05 Part 1"
  [input]
  (->> input
       (u/to-blocks)
       (map u/to-lines)
       (get-valid-page-updates)
       (map get-middle)
       (reduce +)))

(defn- cmp-fn [deps a b]
  (if-let [b-deps (deps b)]
    (not (nil? (b-deps a)))
    false))

(defn- sort-updates [deps page-updates]
  (sort (partial cmp-fn deps)  page-updates))

(defn- fix-updates [deps invalid-updates]
  (map (partial sort-updates deps) invalid-updates))

(defn- get-and-fix-invalid-updates [[rules page-updates]]
  (let [deps (process-rules rules)
        updates (map u/parse-out-longs page-updates)
        invalid-updates (remove (partial is-page-updates-valid? deps) updates)]
    (fix-updates deps invalid-updates)))

(defn part-2
  "Day 05 Part 2"
  [input]
  (->> input
       (u/to-blocks)
       (map u/to-lines)
       (get-and-fix-invalid-updates)       
       (map get-middle)
       (reduce +)))
