(ns aoc-2022.day09
  (:require [aoc-2022.util :as util]
            [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; # Day 9

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day09.clj)

;; https://adventofcode.com/2022/day/9

(def input (util/fetch-input 9))

;; # Part 1

;; Here's a function to [calculate the distance](https://en.wikipedia.org/wiki/Euclidean_distance#Two_dimensions) between two points.
(defn distance [[x1 y1] [x2 y2]]
  (let [x* (* (- x2 x1) (- x2 x1))
        y* (* (- y2 y1) (- y2 y1))]
    (Math/sqrt (+ x* y*))))

;; Distance between two points that does not require a single diagonal move to touch.
(distance [3 1] [1 1])

;; Distance between two points that requires a single diagonal move to touch.
;; The value will be between 2 and 3.
(distance [2 3] [1 1])

;; I can calculate which direction to move in by calculating all of the single moves
;; from a point and then see which one is touching.
(def directions
  (for [x [-1 0 1]
        y [-1 0 1]]
    [x y]))

(defn add-coordinates [c1 c2]
  (mapv + c1 c2))

(add-coordinates [0 0] [-1 1])

;; If the leader and follower are on the same row then it looks like if I choose
;; the direction that if moved in that direction has the smallest distance, that will work.
(let [leader [1 3]
      follower [1 1]]
  (->> directions
       (map (fn [direction]
              (let [new-follower-location (add-coordinates follower direction)]
                {:direction direction
                 :new-location new-follower-location
                 :distance (distance leader new-follower-location)})))
       (clerk/table)))

;; I am going to try the same thing but for diagonal distance. The new location should be `[2, 2]`.
(let [leader [1 2]
      follower [3 1]]
  (->> directions
       (map (fn [direction]
              (let [new-follower-location (add-coordinates follower direction)]
                {:direction direction
                 :new-location new-follower-location
                 :distance (distance leader new-follower-location)})))
       (clerk/table)))

;; Two points are touching if the distance between them is less than 2. This
;; accounts for them being diagonal and on top of each other.
(defn touching? [leader follower]
  (< (distance leader follower) 2))

(touching? [1 2] [3 2])

;; Given the current coordinates of a leader and a follower, this will return
;; the next coordinates of the follower. It calculates the distance in every direction
;; and takes the coordinate that represents the smallest distance. This works
;; for diagonal and non-diagonal case. 
(defn follow-leader [leader follower]
  (if (touching? leader follower)
    follower
    (->> directions
         (map (fn [direction]
                (let [new-location (add-coordinates follower direction)]
                  {:new-location new-location
                   :distance (distance leader new-location)})))
         (apply min-key :distance)
         :new-location)))

;; Test movement on the same row
(follow-leader [0 0] [0 2])

;; Test diagonal movement
(follow-leader [0 0] [1 2])

(def direction->coordinate
  {"L" [0 -1]
   "R" [0 1]
   "U" [-1 0]
   "D" [1 0]})

;; Explode each line into it's natural vector-direction representation.
(def leader-moves
  (->> input
       str/split-lines
       (mapcat (fn [line]
                 (let [[_ direction moves] (re-find #"(.+) (\d+)" line)
                       moves (parse-long moves)]
                   (repeat moves (direction->coordinate direction)))))))

;; Simulate the leader (head) movement and have the tail follow. Keeping tracking of
;; 3 things
;; * Position of the leader/head.
;; * Position of the follower/tail.
;; * Positions of the follower/tail in a set.
(def state
  (let [start [0 0]]
    (reduce (fn [{:keys [leader follower follower-history] :as m} leader-move]
              (let [leader-next-move   (add-coordinates leader leader-move)
                    follower-next-move (follow-leader leader-next-move follower)]
                {:leader leader-next-move
                 :follower follower-next-move
                 :follower-history (conj follower-history follower-next-move)}))
            {:leader start
             :follower start
             :follower-history #{start}}
            leader-moves)))

;; Now I can answer Part 1.
;; >Simulate your complete hypothetical series of motions.
;; How many positions does the tail of the rope visit at least once?
(count (:follower-history state))

;; Done with Part 1 🎉🎉
