(ns aoc-2022.day03
  (:require [aoc-2022.util :as util]
            [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.set :as set]))

;; # Day 3

;; https://adventofcode.com/2022/day/3

(def input
  (util/fetch-input 3))

;; ```
;; A given rucksack always has the same number of items in
;; each of its two compartments, so the first half of the
;; characters represent items in the first compartment,
;; while the second half of the characters represent items
;; in the second compartment.
;; ```

;; Let's confirm each rucksack has an even number of items by
;; selecting only the rucksacks with an odd number and making
;; sure there are zero.
(->> input
     str/split-lines
     (map count)
     (filter odd?)
     count
     zero?)

;; Since we'll need to take a sequence and split it in half,
;; let's make a function do that.

(defn split-rucksack [rucksack]
  (split-at (/ (count rucksack) 2) rucksack))

;; Let's try some examples out.
(split-rucksack [1 2])
(split-rucksack [1 2 3 4])

;; What happens if we have an odd number? It won't really matter since odd numbers won't exist but am curious.
(split-rucksack [1 2 3])
(split-rucksack [1])

;; Now let's split an actual rucksack
(split-rucksack "vJrwpWtwJgWrhcsFMMfFFhFp")

;; We also need to calculate the priority of an item.
(def priority
  (merge
    (zipmap "abcdefghijklmnopqrstuvwxyz" (range 1 (inc 26)))
    (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 27 (inc 52)))))

;; Let's use the examples and see if we have this right.
(def examples
  ["vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"
   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
   "ttgJtRGJQctTZtZT"
   " CrZsJsPPZsGzwwsLwLmpwMDw"])

(clerk/table
  (->> examples
       (map (fn [rucksack]
              (let [[left right] (split-rucksack rucksack)]
                [(apply str left) (apply str right) (set/intersection (set left) (set right))])))
       (into [["Left" "Right" "Common"]])))

;; In the examples, there is only one item per rucksack that is common. Is this true in the official input?
;; Let's calculate the frequencies of the number of common items per rucksack.
(->> input
     str/split-lines
     (map split-rucksack)
     (map (fn [[left right]]
            (set/intersection (set left) (set right))))
     (map count)
     frequencies)

;; *There is only one common item per rucksack.*

;; # Part 1
;; ```
;; What is the sum of the priorities of those item types?
;; ```
;; In the set of common items, take the first since we know there will always be only one.
(->> input
     str/split-lines
     (map split-rucksack)
     (map (fn [[left right]]
            (set/intersection (set left) (set right))))
     (map first) ;; take the first
     (map priority)
     (reduce +))

;; # Part 2
;;```
;; Find the item type that corresponds to the badges of each three-Elf group.
;; What is the sum of the priorities of those item types?
;;```
;; For this next part, we need to group the lines in 3s and then for each of those groups,
;; find the common letters. We can use `partition` for that.

;; Here are the first 10 groups of 3 with their common item.
(let [rows [["Left" "Middle" "Right" "Common"]]]
  (->> input
       str/split-lines
       (partition 3)
       (map (fn [[left middle right]]
              [left middle right
               (set/intersection (set left) (set middle) (set right))]))
       (take 10)
       (into rows)
       (clerk/table {::clerk/width :full})))

;; Now we can calculate the priorities for all of them and sum it up.
(->> input
     str/split-lines
     (partition 3)
     (map (fn [[left middle right]]
            (set/intersection (set left) (set middle) (set right))))
     (map first)
     (map priority)
     (reduce +))

;; Whoo hoo!