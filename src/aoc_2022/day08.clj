(ns aoc-2022.day08
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

;; # Day 8

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day08.clj)

;; https://adventofcode.com/2022/day/8

(def input (util/fetch-input 8))

;; # Part 1

;;>Consider your map; how many trees are visible from outside the grid?

;; ## Discovery
;; The input is a block of numbers. What's the size of the block?

;; Amount in the first row (columns) are:
(def cols
  (->> input
       str/split-lines
       first
       count))

;; And there are this many rows:
(def rows
  (->> input
       str/split-lines
       count))

;; There are this many trees:
(* rows cols)

;; ## Approach

;; For each number in the block, it will either be visible or not visible from the outside.
;; The first approach (dumbest) I can think of is to traverse the grid and then travel
;; to each edge (left, right, up, down). If I get to the edge then it is visible but
;; if I hit a tree that is taller or just as tall, it is not visible and I can also stop
;; traversing.

;; I only need to find at least one direction in which it is visible so if I find it,
;; I don't have to continue looking in other directions. Maybe I'll pick a direction
;; at random.

;; A useful function could be, given a coordinate and a direction give a sequence of all the trees
;; in that direction.

;; Using the example
;; ```
;;  30373
;;  2X512
;;  65332
;;  33549
;;  35390
;;```

;; If we called our hypothetical function `(get-trees coordinate :right)` it would return
;; `512.` And if I called `(get-tree coordinate :down)` it would return `533`.

;; Getting `:left` and `:right` would be straightforward but getting `:up` and `:down`
;; means I may have to transpose the matrix. If I do that, I'll be able to grab slices
;; of the matrix which should make it faster than having to go up and down.

(def example [[3 0  3 7 5]
              [2 9  5 1 2]
              [6 5  3 3 2]
              [3 7  5 4 9]
              [3 5 \X 9 0]])

(get-in example [4 2])

(def example-transposed
  (apply mapv vector example))

(get-in example-transposed [2 4])

;; The above examples are proving to myself that I can transpose the matrix and
;; convert `:left` and `:right` to `:down` and `:up.`

;; Version 1
(defn get-trees [grid [x y] direction]
  (cond
    (= direction :left)
    (let [row (get grid x)]
      (subvec row 0 y))
    
    (= direction :right)
    (let [row (get grid x)]
      (subvec row (inc y) (count row)))
    
    (= direction :down)
    (let [transposed (apply mapv vector grid)
          column (get transposed y)]
      (subvec column (inc x) (count column)))
    
    (= direction :up)
    (let [transposed (apply mapv vector grid)
          column (get transposed y)]
      (vec (reverse (subvec column 0 x))))
    
    :else
    (throw (ex-info "invalid direction" {:direction direction}))))

(get-trees
  example
  [4 4]
  :right)

(get-trees
  example
  [4 4]
  :left)

(get-trees
  example
  [0 4]
  :down)

(get-trees
  example
  [2 2]
  :up)

;; Version 2

;; I am going to parse the input into our grid and in addition will tranpose the grid
;; and have it ready to go in memory. This will save us a lot of time.
(def grid
  (->> input
       str/split-lines
       (mapv (fn [s]
               (mapv (fn [c]
                       (parse-long (Character/toString c)))
                     s)))))

(def transposed-grid
  (apply mapv vector grid))

(defn get-trees-v2 [[x y] direction]
  (let [grid* (if (#{:left :right} direction)
                grid
                transposed-grid)]
    (cond
      (= direction :left)
      (let [row (get grid* x)]
        (subvec row 0 y))
      
      (= direction :right)
      (let [row (get grid* x)]
        (subvec row (inc y) (count row)))
      
      (= direction :down)
      (let [column (get grid* y)]
        (subvec column (inc x) (count column)))
      
      (= direction :up)
      (let [column (get grid* y)]
        ;; from 0 to x will get me the right
        ;; elements but since I'm looking up,
        ;; I need to reverse them
        (vec (reverse (subvec column 0 x))))
      
      :else
      (throw (ex-info "invalid direction" {:direction direction})))))

;; It's not visible if there is at least one tree that is the same height or greater.
(defn visible? [tree other-trees]
  (not
    (some
      (fn [other-tree]
        (>= other-tree tree))
      other-trees)))

;; I don't need to check every possibility but it is fast enough so let's do this.
(def all-possibilities
  (for [x         (range (count grid))
        y         (range (count (first grid)))
        direction [:up :down :left :right]]
    [x y direction]))

(->> all-possibilities
     (map (fn [[x y direction]]
            (let [tree (get-in grid [x y])
                  other-trees (get-trees-v2 [x y] direction)]
              ;; I will know which coordinate is visible from
              ;; every direction. Since I only need to know if
              ;; at least one direction is visibile, this is
              ;; excessive.
              {:coordinates [x y]
               :direction direction
               :visible? (visible? tree other-trees)})))
     (filter :visible?)
     (map :coordinates)
     set ;; put it in a set to uniquify them
     count)

;; Done! 🎉🎉
