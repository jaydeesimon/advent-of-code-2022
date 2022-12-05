(ns aoc-2022.day01
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; https://adventofcode.com/2022/day/1

;; # Day 1's Input
(def input
  (util/fetch-input 1))

;; How many elves are there?
(->> input str/split-lines count)

;; # Part 1

;;```
;; Find the Elf carrying the most Calories.
;; How many total Calories is that Elf carrying?
;; ```

;; Group the elves and then sum their calories, sort it descending and take the first.
(def elf-calories-sorted-descending
  (->> input
       str/split-lines
       (partition-by #(= % ""))
       (remove #(= % '("")))
       (map (fn [group]
              (let [calories (map parse-long group)]
                (reduce + calories))))
       (sort >)))

(first elf-calories-sorted-descending)

;; # Part 2

;;```
;; Find the top three Elves carrying the most Calories.
;; How many Calories are those Elves carrying in total?
;;```

;; Take the top 3 and sum them up.

(reduce + (take 3 elf-calories-sorted-descending))