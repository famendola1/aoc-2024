(ns advent-of-code.day17
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn- combo [[ra rb rc] operand]
  (condp = operand
    0 operand
    1 operand
    2 operand
    3 operand
    4 ra
    5 rb
    6 rc))

(defn- adv [[ra rb rc :as regs] operand]
  [(bit-shift-right ra (combo regs operand)) rb rc])

(defn- bxl [[ra rb rc] operand]
  [ra (bit-xor rb operand) rc])

(defn- bst [[ra rb rc :as regs] operand]
  [ra (bit-and (combo regs operand) 2r111) rc])

(defn- jnz [[ra] idx operand]
  (if (zero? ra) (inc idx) operand))

(defn- bxc [[ra rb rc] _]
  [ra (bit-xor rb rc) rc])

(defn- out [regs operand]
  (bit-and (combo regs operand) 2r111))

(defn- bdv [[ra rb rc :as regs] operand]
  [ra (bit-shift-right ra (combo regs operand)) rc])

(defn- cdv [[ra rb rc :as regs] operand]
  [ra rb (bit-shift-right ra (combo regs operand))])

(def instructions-map
  {0 adv
   1 bxl
   2 bst
   4 bxc
   6 bdv
   7 cdv})

(defn- execute [regs instructions]
  (let [instructions (partition 2 (map int instructions))]
    (loop [regs regs
           ins-ind 0
           [[opcode operand :as ins] & rem] instructions 
           res []]
      (cond (not (seq ins)) res
            (= 3 opcode)
            (let [new-ind (jnz regs ins-ind operand)]
              (recur regs new-ind (drop new-ind instructions) res))
            (= 5 opcode) (recur regs (inc ins-ind) rem (conj res (out regs operand)))
            :else (recur ((instructions-map opcode) regs operand) (inc ins-ind) rem res)))))

(defn part-1
  "Day 17 Part 1"
  [input]
  (->> input
       (u/to-blocks)
       (map u/parse-out-longs)       
       (apply execute)
       (str/join \,)))

(defn- find-identity [instructions]
  (loop [i 0]
    (let [output (execute [i 0 0] instructions)
          to-check(drop (- (count instructions) (count output)) instructions)]
      (cond (= output instructions) i
            (= output to-check) (recur (bit-shift-left i 3))
            :else (recur (inc i))))))

(defn part-2
  "Day 17 Part 2"
  [input]
  (->> input
       (u/to-blocks)
       (map u/parse-out-longs)
       (last)
       (find-identity)))
