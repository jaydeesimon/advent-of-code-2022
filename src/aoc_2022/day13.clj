(ns aoc-2022.day13
  (:require [aoc-2022.util :as util]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as str]))

;; # Day 13

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day13.clj)

;; https://adventofcode.com/2022/day/13

(def input (util/fetch-input 13))

;; # Part 1

(def sample-pair "[[[[8,7,8,5,4],6,[4,6]]],[6,[[]]],[]]")

;; Should be able to convert the string straight to EDN.
(edn/read-string sample-pair)

;; Confirmed that each line is valid EDN. That's convenient.
(->> input
     str/split-lines
     (remove str/blank?)
     (map edn/read-string))
