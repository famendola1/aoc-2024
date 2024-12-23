(ns advent-of-code.day21
  (:require [advent-of-code.utils :as u]
            [clojure.string :as string]))

(def key-presses
  {\A {\A "A"
       \< "v<<A"
       \^ "<A"
       \v "v<A"
       \> "vA"
       \0 "<A"
       \1 "^<<A"
       \3 "^A"
       \4 "^^<<A"
       \5 "<^^A"
       \6 "^^A"
       \8 "<^^^A"
       \9 "^^^A"}
   \v {\A "^>A"
       \v "A"
       \< "<A"
       \> ">A"
       \^ "^A"}
   \< {\A ">>^A"
       \< "A"
       \v ">A"
       \> ">>A"
       \^ ">^A"}
   \> {\A "^A"
       \> "A"
       \< "<<A"
       \v "<A"
       \^ "^<A"}
   \^ {\A ">A"
       \^ "A"
       \> "v>A"
       \< "v<A"
       \v "vA"}
   \0 {\2 "^A"
       \A ">A"}
   \1 {\A ">>vA"
       \7 "^^A"}
   \2 {\A "v>A" 
       \6 ">^A"
       \9 "^^>A"}
   \3 {\A "vA"
       \4 "<<^A"
       \7 "<<^^A"}
   \4 {\1 "vA"
       \5 ">A"}
   \5 {\6 ">A"
       \8 "^A"}
   \6 {\A "vvA"
       \7 "<<^A"}
   \7 {\0 ">vvvA"
       \9 ">>A"}
   \8 {\0 "vvvA"
       \2 "vvA"
       \3 "vv>A"}
   \9 {\A "vvvA"
       \8 "<A"}})

(defn- press-key-pad [keys]
  (mapcat #(seq (get-in key-presses %))
          (partition 2 1 (cons \A keys))))

(defn- press-door-code [robots code]
  (reduce (fn [keys _] (press-key-pad keys))
          code
          (range (inc robots))))

(def expand-path
  (memoize
   (fn [robots curr next]
     (let [presses (get-in key-presses [curr next])]
       (if (zero? robots)
         presses
         (apply str
                (mapcat #(apply expand-path (dec robots) %)
                        (partition 2 1 (str "A" presses)))))))))

(defn- press-door-code-alt [robots code]
  (apply str (mapcat #(apply expand-path robots %)
                     (partition 2 1 (str "A" code)))))

(defn- complexity [robots code]
  (let [num (first (u/parse-out-longs code))]
    (* num (count (press-door-code-alt robots code)))))

(defn part-1
  "Day 21 Part 1"
  [input]
  (->> input
       (u/to-lines)
       (map (partial complexity 2))
       (apply +)))

(defn part-2
  "Day 21 Part 2"
  [input]
  (->> input
       (u/to-lines)
       (map (partial complexity 25))
       (apply +)))
