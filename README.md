# advent-2024-clojure

This is my code for the 2024 [Advent of Code](https://adventofcode.com/20XX),
all solutions in [Clojure](https://clojure.org/).

All code is under the `src` directory. Each solution-file is named `dayNN.clj`
and contains both puzzle solutions for that day. These are the
publically-facing functions `part-1` and `part-2`. These files are the code
*exactly as I used it to solve and submit the answers*. If I revisit any of the
days and try to clean up or optimize the solutions, that work will be in a
separate file that will be named `dayNNbis.clj`
(["bis"](https://www.merriam-webster.com/dictionary/bis)). I may go back and
comment code after the fact, when I'm not racing the clock.

## Stats

Number of answers correct on first submission: -/- (--%)

Highest finish for first half: -

Highest finish for second half: -

## Usage

This project is managed with [Leiningen](https://leiningen.org/). Running the
following will download any dependencies and start a REPL:

```
lein repl
```

# Advent of Code Clojure Basis

Starter pack for doing [Advent of Code](https://www.adventofcode.com) in
Clojure. This is based largely on Mitchell Hanburg's [Advent of Code
Clojure Starter](https://github.com/mhanberg/advent-of-code-clojure-starter).
I've rewritten a lot of the structural stuff, and re-done the `core.clj` code
so that it only loads the specific day's namespace and dynamically looks up the
appropriate function before running it.

Uses [lein](https://github.com/technomancy/leiningen).

## Usage

There are 25 namespaces in `src/advent_of_code`, a number of utility functions
in the `utils` namespace, and a launcher function in `core`. The recommended
process is:

1. Save the puzzle input into `resources` as `dayNN.txt`
1. Code the solution in `src/advent_of_code/dayNN.clj`
1. Test in the REPL (preferably using [CIDER](https://cider.mx/) in Emacs)
1. When ready, run `lein run <day> <part>` to run with the puzzle input

Previously, I had 25 zero-length files in `resources` with the names already
set. The intention was to have them there for easy overwriting when saving the
data from the web browser. But I don't want to save AoC data in my public
repos, so I have this mechanism set up.

## Namespace Templates

Each of the 25 namespaces (including day 25 which generally only has a part 1)
are essentially identical other than the day-number in the `ns` declaration:

```clojure
(ns advent-of-code.day01
  (:require [advent-of-code.utils :as u]))

(defn part-1
  "Day 01 Part 1"
  [input]
  (->> input))

(defn part-2
  "Day 01 Part 2"
  [input]
  (->> input))
```

The `advent-of-code.utils` module is `require`'d and bound to the prefix `u/`.
Other modules can be added to the `require` form as needed. The template
`project.clj` provided here includes several third-party Clojure packages that
I find I use fairly often:

* `org.clojure/math.numeric-tower`
* `org.clojure/math.combinatorics`
* `org.clojure/core.match`
* `org.clojure/data.priority-map`

The `input` parameter that each puzzle-part receives is the whole of the text
file that is the puzzle input. It is read from the file in `resources` that
gets saved by the user. It is not split into lines or stripped of newline
characters, etc. That is left to the puzzle code to do as needed. The `utils`
file has a variety of functions for parsing/managing the input.

## Checklist

Once this template is used to create a new repo, do the following:

1. Edit `project.clj` to change the `:url` and `:description` properties
2. Edit `LICENSE` to change `20XX` to the current year
3. Edit this file to change `20XX` to the current year
4. Delete everything from second level-1 header ("Advent of Code Clojure Basis")
   to the end
