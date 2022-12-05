(ns aoc-2022.day04
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [clojure.set :as set]))

;; # Day 4

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day04.clj)

;; https://adventofcode.com/2022/day/4

(def input (util/fetch-input 4))

;; Let's take a look at the top 20 lines in the input.
(let [top-n-lines (->> input
                       str/split-lines
                       (take 20))]
  (clerk/md
    (str "```\n" (str/join "\n" top-n-lines) (str "\n```"))))

;; What's the largest number in the input? We can parse the line
;; using a regex.
(def re #"(\d+)-(\d+),(\d+)-(\d+)")

;; Test our regex out.
(re-find re "17-43,43-43")

(defn parse-line [line]
  (map parse-long (take-last 4 (re-find re line))))

(parse-line "17-43,43-43")

;; Now that we can parse, let's find the largest number in our input.
(->> input
     str/split-lines
     (map parse-line)
     (mapcat identity)
     (reduce max))

;; And what's the smallest?
(->> input
     str/split-lines
     (map parse-line)
     (mapcat identity)
     (reduce min))

;; The numbers only go from 1-99 so it will be safe to use `range`.

;; # Part 1
;; ```
;; In how many assignment pairs does one range fully contain the other?
;; ```

;; A range contains another range if the difference between two ranges is the empty set.
;; For example, the 3-4 is fully contained in 1-4 so their is no difference.
(set/difference #{3 4} #{1 2 3 4})

;; Let's try the example above: 2-8 and 3-7. Does 2-8 fully contain 3-7?
(set/difference (set (range 3 (inc 7)))
                (set (range 2 (inc 8))))

;; We can make a function to answer these questions based on `set/difference` and `range`.
(defn does-left-fully-contain-right? [l1 l2 r1 r2]
  (empty? (set/difference (set (range r1 (inc r2)))
                          (set (range l1 (inc l2))))))

;; Does 2-8 fully contain 3-7?
(does-left-fully-contain-right? 2 8 3 7)

;; Does 3-7 fully contain 2-8?
(does-left-fully-contain-right? 3 7 2 8)

;; Let's count how many assignments have ranges that contain each other?
(->> input
     str/split-lines
     (map parse-line)
     (map (fn [[l1 l2 r1 r2]]
            ;; make sure to check both sides
            (if (or (does-left-fully-contain-right? l1 l2 r1 r2)
                    (does-left-fully-contain-right? r1 r2 l1 l2))
              1
              0)))
     (reduce +))

;; # Part 2
;; ```
;; In how many assignment pairs do the ranges overlap?
;; ```
;; For part 2, we only need to see if there is any lap. We can use `set/intersection` for that.
;; If the resulting set has something in it then it overlaps.
(defn does-left-overlap-with-right? [l1 l2 r1 r2]
  (seq (set/intersection (set (range r1 (inc r2)))
                         (set (range l1 (inc l2))))))

;; Now we can do the same thing as above but using `does-left-overlap-with-right?`.
(->> input
     str/split-lines
     (map parse-line)
     (map (fn [[l1 l2 r1 r2]]
            ;; make sure to check both sides
            (if (or (does-left-overlap-with-right? l1 l2 r1 r2)
                    (does-left-overlap-with-right? r1 r2 l1 l2))
              1
              0)))
     (reduce +))

;; Done! 🎉🎉