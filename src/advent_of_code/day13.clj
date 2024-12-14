(ns advent-of-code.day13
  (:require [advent-of-code.utils :as u]))
;; System of linear equations
;; |xg|   |xa ya|   |A|
;; |yg| = |xb yb| * |B|
;;
;; D = xa*yb - ya*xb
;; |A|   |yb/D  -ya/D|   |xg|
;; |B| = |-xb/D  xa/D| * |yg|
;;
;; Prize is only reachable when A and B are integers.
(defn find-prize [k [xa ya] [xb yb] [xg yg]]
  (let [B (/ (- (* xa (+ yg k)) (* (+ xg k) ya)) (- (* xa yb) (* xb ya)))
        A (/ (- (* xb (+ yg k)) (* (+ xg k) yb)) (- (* xb ya) (* xa yb)))]
    (when (and (not (ratio? A)) (not (ratio? B)))
      (+ (* 3 A) B))))

(defn part-1
  "Day 13 Part 1"
  [input]
  (->> input
       (u/to-blocks)
       (map (comp (partial partition 2) u/parse-out-longs))
       (keep (partial apply (partial find-prize 0)))
       (reduce +)))

(defn part-2
  "Day 13 Part 2"
  [input]
  (->> input
       (u/to-blocks)
       (map (comp (partial partition 2) u/parse-out-longs))
       (keep (partial apply (partial find-prize 10000000000000)))
       (reduce +)))
