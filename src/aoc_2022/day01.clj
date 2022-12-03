(ns aoc-2022.day01
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

;; https://adventofcode.com/2022/day/1

;; # Day 1's Input
(def input
  (util/fetch-input 1))

(def elf-calories-sorted-descending
  (->> input
       str/split-lines
       (partition-by #(= % ""))
       (remove #(= % '("")))
       (map (fn [group]
              (let [calories (map #(Long/parseLong %) group)]
                (reduce + calories))))
       (sort >)))

;; # Part 1
(first elf-calories-sorted-descending)

;; # Part 2
(reduce + (take 3 elf-calories-sorted-descending))