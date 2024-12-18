(ns advent-of-code.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :refer [priority-map-keyfn]])
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(defn read-input
  "Read in the content of the given day-file and return as a blob"
  [day]
  (slurp (if (str/starts-with? day "/") day (io/resource day))))

(defn to-blocks
  "Turn a blob (probably from `slurp`) into a seq of blocks"
  [input]
  (str/split input #"\n\n"))

(defn to-lines
  "Turn a blob or block into a seq of lines"
  [input]
  (str/split-lines input))

(defn to-matrix
  "Turn a blob (or block) into a vector of vectors"
  [input]
  (->> input
       to-lines
       (mapv vec)))

(defn parse-out-longs
  "Parse out all numbers in `line` that are integers (longs)"
  [line]
  (map parse-long (re-seq #"[-+]?\d+" line)))

(defn parse-ranges
  "Parse each string of input as a range in the form 'M-N'"
  [ranges]
  (->> ranges
       (map #(str/replace % "-" " "))
       (map parse-out-longs)))

(defn manhattan-dist
  "Calculate the Manhattan Distance between two points."
  [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (last  p1) (last  p2)))))

(defn first-duplicate
  "Find first element of collection that is a duplicate"
  [coll]
  (reduce (fn [acc elt]
            (if (get acc elt)
              (reduced elt)
              (assoc acc elt true)))
          {} coll))

;; Like the core time macro, but rather than printing the elapsed time it
;; returns a list of (result, time). Returned value is in milliseconds.
(defmacro time-it [expr]
  `(let [start# (. System (nanoTime))
         ret#   ~expr
         end#   (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (list ret# end#)))

(defn tee
  "Like 'tap' or 'tee', show the value of expr before returning it"
  [expr]
  (print expr "\n")
  expr)

;; Taken from https://stackoverflow.com/a/3266877/6421
;;
;; Get matches for a given regexp *and* their position within the string.
(defn re-pos
  "Return a list of pairs of (index, string) for all matches of `re` in `s`"
  [re s]
  (loop [m (re-matcher re s), res ()]
    (if (.find m)
      (recur m (cons (list (.start m) (.group m)) res))
      (reverse res))))

;; Lazy sequence of primes, taken from Project Euler code repo
(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
          (fn primes-from [n [f & r]]
            (if (some #(zero? (rem n %))
                      (take-while #(<= (* % %) n) primes))
              (recur (+ n f) r)
              (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn factorize
  "Determine all prime factors of n"
  [n]
  (loop [x n, [p & ps] primes, factors []]
    (cond
      (= 1 x)           factors
      (zero? (mod x p)) (recur (/ x p) primes (conj factors p))
      :else             (recur x ps factors))))

;; https://stackoverflow.com/questions/10347315/matrix-transposition-in-clojure
(defn transpose [m]
  (apply mapv vector m))

;; Taken from https://github.com/narimiran/AdventOfCode2023/blob/main/clojure/aoc.clj
(defn gcd
  ([] 1)
  ([x] x)
  ([a b] (if (zero? b) a
             (recur b (mod a b)))))

(defn lcm
  ([] 1)
  ([x] x)
  ([a b] (/ (* a b) (gcd a b))))

;; Taken from https://gist.github.com/jizhang/4325757?permalink_comment_id=2196746#gistcomment-2196746
(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn create-field
  "Create a NxM field as a matrix (vector of vectors). Fill with `with` or nil"
  [N M & [with]]
  (if (or (seq? with) (vector? with))
    ;; Ignore N/M and treat each element of `with` as a row in the field
    (mapv vec with)
    ;; Otherwise, use the value of `with` itself (which may be nil)
    (vec (repeat M (vec (repeat N with))))))

(defn display
  "Display a matrix of characters, as if on a terminal or similar"
  [lines]
  (println (str/join "\n" (map #(str/join %) lines))))

;; https://stackoverflow.com/questions/24553524/how-to-drop-the-nth-item-in-a-collection-in-clojure
(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn neighbors [[row col]]
  [{:coord [(dec row) (dec col)] :dir :up-left}
   {:coord [(dec row) col] :dir :up}
   {:coord [(dec row) (inc col)] :dir :up-right}
   {:coord [row (dec col)] :dir :left}
   {:coord [row (inc col)] :dir :right}
   {:coord [(inc row) (dec col)] :dir :down-left}
   {:coord [(inc row) col] :dir :down}
   {:coord [(inc row) (inc col)] :dir :down-right}])

(defn cardinal-neighbors [coord]
  (filter #(#{:up :down :left :right} (:dir %)) (neighbors coord)))

(defn out-of-bounds [row-bound col-bound]
  (fn [[row col]]
    (or (>= row row-bound) (< row 0)
        (>= col col-bound) (< col 0))))

(defn debug [x & msgs]
  (when msgs
    (apply println msgs))
  (pprint x)
  x)

(defn matrix->map [matrix]
  (into {}
        (for [row (range (count matrix))
              col (range (count (first matrix)))
              :let [coord [row col]]]
          [coord (get-in matrix coord)])))

(defn dissoc-by [pred m]
  (apply dissoc m (filter pred (keys m))))

(defn dissoc-by-vals [pred m]
  (reduce-kv (fn [m k v] (if-not (pred v) (assoc m k v) m)) {} m))

(defn- merge-costs [[curr-cost curr-prevs :as curr] [new-cost new-prevs :as new]]
  (cond (= curr-cost new-cost) [curr-cost (set/union curr-prevs new-prevs)]
        (< new-cost curr-cost) new
        :else curr))

(def ^:private invert-dir
  {:left :right
   :up :down
   :right :left
   :down :up})

(defn dijkstra
  "Dijkstra's algorithm for finding the minimum path between two positions in
  a matrix. Positions consist of a coordinate and a direction.
  
  - start: the start position [coordinate, direction]
  - target: the target coordinate. This does not include the direction.
  - nbrs-fn: a function that takes a position and returns a map of positions and
  the cost of travelling to that position.
  - alt-targets: specifies alternate nodes that are also targets. This is
  useful when node have an associated state that don't matter for the target.
  For example, if you're doing this search on a matrix where the direction
  you're travelling in is important."  
  [start target nbrs-fn & {:keys [alt-targets] :or {alt-targets #{}}}]  
  (loop [q (priority-map-keyfn first start [0 #{}])
         res {}]
    (let [[node [cost :as cost-and-prevs]] (peek q)]
      (cond (not (seq q)) res
            (or (= node target) (alt-targets node))
            (assoc res node cost-and-prevs)
            :else
            (let [new-costs (->> (nbrs-fn node)
                                 ;; skip nodes we've visited
                                 (dissoc-by #(res %))
                                 (#(update-vals % (partial + cost))))]
              (recur (merge-with merge-costs
                                 (pop q)
                                 (update-vals new-costs #(vector % (set [node]))))
                     (assoc res node cost-and-prevs)))))))

(defn dfs
  "Depth First Search Algorithm
  - start: the start node
  - target: the target node
  - nbrs-fn: a functions that takes a node and returns a list of its neighboring
  nodes."
  [start target nbrs-fn]  
  (loop [q [[start (set nil)]]
         res {}]
    (let [[curr path] (peek q)]
      (cond (nil? curr) res
            (= curr target) (assoc res target (conj path target))
            (res curr) (recur (pop q) res)
            :else (recur (vec (concat (pop q)
                                      (map #(vector % (conj path curr))
                                           (nbrs-fn curr))))
                         (assoc res curr (conj path curr)))))))
