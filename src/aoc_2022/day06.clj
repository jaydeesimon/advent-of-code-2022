(ns aoc-2022.day06
  (:require [aoc-2022.util :as util]
            [nextjournal.clerk :as clerk]))

;; # Day 6

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day06.clj)

;; https://adventofcode.com/2022/day/6

(def input (util/fetch-input 6))

;; # Part 1

;; Here is the important piece for Part 1:
;; >The device will send your subroutine a datastream buffer (your puzzle input); <strong>your subroutine needs to identify
;; the first position where the four most recently received characters were all different.</strong>
;; Specifically, it needs to report the number of characters from the beginning of the buffer
;; to the end of the first such four-character marker.

;; We need to find the first 4 characters that are all different and we also need to know where in the
;; string these characters are located.

;; This may be super slow but my instinct is to walk through the string one character at a time,
;; take the next four characters. If they are different we are done else keep going until you get to the end.

;; I will try this with a loop since we'll need to keep track of where we are.

(let [marker-size 4]
  (loop [begin-index 0]
    (let [end-index (min (+ begin-index marker-size) (count input))
          marker    (.substring input begin-index end-index)]
      (if (= (count (set marker)) marker-size)
        (+ begin-index marker-size)
        (recur (inc begin-index))))))

;; Done! 🎉

;; # Part 2

;; >A start-of-message marker is just like a start-of-packet marker, except it consists of 14 distinct
;; characters rather than 4.

;; This feels like a trick, in the sense, that if my first part was done in a naive way, it won't work
;; well for 14 characters. I will try anyway.

(let [marker-size 14]
  (loop [begin-index 0]
    (let [end-index (min (+ begin-index marker-size) (count input))
          marker    (.substring input begin-index end-index)]
      (if (= (count (set marker)) marker-size)
        (+ begin-index marker-size)
        (recur (inc begin-index))))))

;; That seemed just as fast 🤷 🎉