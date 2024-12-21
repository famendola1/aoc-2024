(ns advent-of-code.day21
  (:require [advent-of-code.utils :as u]
            [clojure.string :as string]))

(def key-presses
  {\A {\A ""
       \< "v<<"
       \^ "<"
       \> "v"
       \0 "<"
       \1 "^<<"
       \3 "^"
       \4 "^^<<"
       \9 "^^^"}
   \< {\A ">>^"
       \< ""
       \v ">^"
       \> ">>"
       \^ ">^"}
   \> {\A "^"
       \> ""
       \< "<<"
       \v "<"
       \^ "<^"}
   \^ {\A ">"
       \^ ""
       \> "v>"
       \< "v<"
       \v "v"}
   \0 {\2 "^"
       \A ">"}
   \1 {\7 "^^"}
   \2 {\9 ">^^"}
   \3 {\7 "<<^^"}   
   \4 {\5 ">"}
   \5 {\6 ">"}
   \6 {\A "vv"}
   \7 {\9 ">>"}
   \8 {\0 "vvv"}
   \9 {\A "vvv"
       \8 "<"}})

(defn- press-key-pad [keys]
  (str (string/join (u/debug (map #(get-in key-presses %) (partition 2 1 keys))) "A") "A"))

(defn- press-door-code [robots code]
  (take (inc robots) (iterate press-key-pad (press-key-pad code))))

(defn- complexity [robots code]
  (let [num (u/parse-out-longs code)]
    (u/debug (press-door-code robots (str "A" code)))
    0))

(defn part-1
  "Day 21 Part 1"
  [input]
  (->> input
       (u/to-lines)
       (map (partial complexity 0))
       (apply +))  
  "Implement this part")

(defn part-2
"Day 21 Part 2"
[input]
"Implement this part")
