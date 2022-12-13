(ns aoc-2022.day10
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; # Day 10

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day10.clj)

;; https://adventofcode.com/2022/day/10

(def input (util/fetch-input 10))

(def sample-input
  ["noop"
   "addx 3"
   "addx -5"])

(defn parse-add-instruction [s]
  (parse-long (second (str/split s #" "))))

;; I can expand each instruction into its cycles and use a function
;; to represent what the value should be for the next cycle. The `identity`
;; function can be used to represent a noop.
(defn generate-instruction-fns [instructions]
  (->> instructions
       (mapcat (fn [instruction]
                 (if (str/starts-with? instruction "noop")
                   [identity]
                   (let [n (parse-add-instruction instruction)]
                     [identity
                      (fn [x] (+ x n))]))))))

(def sample-instruction-fns
  (generate-instruction-fns sample-input))

;; Test executing the basic sample instructions.
(reduce (fn [xs instruction-fn]
          (let [current-x (last xs)]
            (conj xs (instruction-fn current-x))))
        [1]
        sample-instruction-fns)

(def sample-input-2
  (str/split-lines "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"))

(def xs-sample-input-2
  (reduce (fn [xs instruction-fn]
            (let [current-x (last xs)]
              (conj xs (instruction-fn current-x))))
          [1]
          (generate-instruction-fns sample-input-2)))

(let [cycles [20 60 100 140 180 220]
      get-cycle-fns (map (fn [cycle]
                           (fn [cycles] (get cycles (dec cycle))))
                         cycles)
      get-cycle-fn (apply juxt get-cycle-fns)]
  (get-cycle-fn xs-sample-input-2))

(defn execute-instructions [instructions]
  (reduce (fn [xs instruction-fn]
            (let [current-x (last xs)]
              (conj xs (instruction-fn current-x))))
          [1]
          (generate-instruction-fns instructions)))

(defn signal-strength-per-cycle [instructions]
  (let [executed-cycles (execute-instructions instructions)
        interesting-cycles [20 60 100 140 180 220]]
    (->> interesting-cycles
         (map (fn [interesting-cycle]
                (* interesting-cycle
                   (get executed-cycles (dec interesting-cycle))))))))

(signal-strength-per-cycle sample-input-2)

;; Test it on the longer sample input.
(apply + (signal-strength-per-cycle sample-input-2))

;; Now let's do it for the real input.
(let [instructions (str/split-lines input)]
  (apply + (signal-strength-per-cycle instructions)))

;; Done! 🎉🎉

;; # Part 2

;; Verify that there are 40 x 6 cycles in our input
(->> input
     str/split-lines
     execute-instructions
     (partition 40)
     (map count))

;; I will see what the smallest x and largest x is for every cycle.
(->> input
     str/split-lines
     execute-instructions
     (apply (juxt min max)))
;; ☝️ Weird that there's a -1 but I will interpret that as meaning the middle of the sprite
;; is off the screen and the right of it is in position 0.

;; If the current position intersects with the value of `x`, the middle of the sprite
;; then I will draw an `X` and not a `.` (period).
(defn intersect? [position x]
  (or (= x position)
      (= (dec x) position)
      (= (inc x) position)))

;; A function to convert a sequence of cycles and convert them into a rendered row.
(defn draw40 [cycles]
  (map-indexed (fn [position x]
                 (if (intersect? position x)
                   \#
                   \.))
               cycles))

;; Execute the instructions, break them into groups of 40 and then render
;; each line. 
(def rendered-crt
  (->> input
       str/split-lines
       execute-instructions
       (partition 40)
       (map draw40)
       (map (partial apply str))
       (str/join "\n")))

(clerk/md
  (format "```\n%s\n```" rendered-crt))

;; Looks like `PHLHJGZA`

;; Done! 🎉🎉
