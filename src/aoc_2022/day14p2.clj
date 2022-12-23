(ns aoc-2022.day14p2
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; # Day 14 Part 2

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day14p2.clj)

;; https://adventofcode.com/2022/day/14#part2

(def input (util/fetch-input 14))

;; >Using your scan, simulate the falling sand until the source of the sand becomes blocked.
;; How many units of sand come to rest?

;;>You don't have time to scan the floor, so assume the floor is an infinite horizontal line with a y
;; coordinate equal to two plus the highest y coordinate of any point in your scan.

(defn position-available? [grid position floor-y]
  (and (nil? (get grid position))
       (let [[_ position-y] position]
         (< position-y floor-y))))

(position-available? {} [0 14] 15)

(def down [0 1])
(def down-left [-1 1])
(def down-right [1 1])

(def possible-directions [down down-left down-right])

(defn next-move [grid position floor-y]
  (loop [possible-directions* possible-directions]
    (let [possible-direction (first possible-directions*)]
      (if possible-direction
        (let [next-move* (mapv + possible-direction position)]
          (if (position-available? grid next-move* floor-y)
            next-move*
            (recur (rest possible-directions*))))
        position))))

;; This is the reason I wanted to do Day Part 2 in a separate namespace. I needed a faster way to find the resting
;; position of a piece of sand. The old way moved a piece of sand and then checked the entire state. Way too slow.
;; Also, I can only move one piece of sand at a time so there's no point in doing that. It worked for Part 1 but not
;; for Part 2. For Part 2, let's loop through the path for a piece of sand and find the resting spot right away.
(defn find-resting-position [grid start floor-y]
  (loop [current start]
    (let [next-move* (next-move grid current floor-y)]
      (if (= next-move* current)
        next-move*
        (recur next-move*)))))

(find-resting-position {:grid {[0 1] :rock
                               [-1 1] :rock}} [0 0] 500)


;; Given a magnitude, get the unit value.
(defn unit-value [magnitude]
  (cond
    (pos? magnitude) 1
    (neg? magnitude) -1
    :else 0))

(defn points-in-line [start end]
  (let [distance (mapv - end start)
        direction (mapv unit-value distance)]
    (loop [points #{}
           current start]
      (if (= current end)
        (conj points current)
        (recur (conj points current)
               (mapv + current direction))))))

(def lines
  (->> input
       str/split-lines
       (map (fn [line]
              (str/split line #" -> ")))
       (mapcat (fn [line-points]
                 (->> (map (fn [line-point]
                             (let [[_ x y] (re-find #"(\d+),(\d+)" line-point)]
                               [(parse-long x) (parse-long y)]))
                           line-points)
                      (partition 2 1))))))

(defn initialize-grid [lines]
  (reduce (fn [grid [p1 p2]]
            (reduce (fn [grid* p*]
                      (assoc grid* p* :rock))
                    grid
                    (points-in-line p1 p2)))
          {}
          lines))

(def initial-grid (initialize-grid lines))

;; The floor 2 units below the lowest rock.
(def floor-y (->> initial-grid
                  keys
                  (map second)
                  (apply max)
                  (+ 2)))

(find-resting-position initial-grid [500 0] floor-y)

;; My function to start filling the sand. It will stop when the starting position becomes a sand particle.
(defn fill-sand [grid start floor-y]
  (loop [grid* grid
         iterations 1]
    (when (zero? (mod iterations 10000))
      (println iterations "iterations"))
    (cond
      (> iterations 50000)
      grid*

      (= (get grid* [500 0]) :sand)
      (do
        (println "There's sand at the source!")
        grid*)

      :else
      (let [resting-position (find-resting-position grid* start floor-y)]
        (recur (assoc grid* resting-position :sand)
               (inc iterations))))))

;; My ugly function to print out a grid.
(defn grid->string
  ([grid]
   (grid->string grid nil))
  ([grid floor-y]
   (let [xmin           (->> (keys grid)
                             (map first)
                             (apply min))
         xmax           (->> (keys grid)
                             (map first)
                             (apply max))
         ymin           (->> (keys grid)
                             (map second)
                             (apply min))
         ymax           (->> (keys grid)
                             (map second)
                             (apply max))
         ymax           (if floor-y
                          (+ 2 ymax)
                          ymax)
         partition-size (inc (- xmax xmin))]
     (->> (for [y (range ymin (inc ymax))
                x (range xmin (inc xmax))
                :let [e (get grid [x y])
                      c (cond
                          (and (some? floor-y) (= y floor-y)) \#
                          (= e :rock) \#
                          (= e :sand) \o
                          (= e :sand-at-rest) \o
                          :else \.)]]
            c)
          (partition partition-size)
          (map #(apply str %))
          (str/join "\n")))))

;; Print the grid without any sand in it, only rocks.
^{:nextjournal.clerk/width :full}
(clerk/md
  (format "```\n%s\n```" (grid->string initial-grid floor-y)))

;; Fill up the grid with sand!
(def grid-with-sand
  (fill-sand initial-grid [500 0] floor-y))

;; Here's the grid full of sand. Pretty cool!
^{:nextjournal.clerk/width :full}
(clerk/md
  (format "```\n%s\n```" (grid->string grid-with-sand floor-y)))


;; How much sand?
(->> grid-with-sand
     vals
     (filter #(= % :sand))
     count)

;; Done! 🎉🎉🎉🪨🪨🪨