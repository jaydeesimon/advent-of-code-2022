(ns aoc-2022.day15
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

;; # Day 15

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day15.clj)

;; https://adventofcode.com/2022/day/15

(def input (util/fetch-input 15))

;; # Part 1

;; >Consult the report from the sensors you just deployed. In the row where y=2000000,
;; how many positions cannot contain a beacon?

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(manhattan-distance [8 7] [2 10])

;; Define a map of sensor points to their closest beacon points.
(def sensor->beacon
  (->> input
       str/split-lines
       (map (fn [line]
              (let [re #"Sensor at x=([-\d]+), y=([-\d]+): closest beacon is at x=([-\d]+), y=([-\d]+)"
                    [x1 y1 x2 y2] (map parse-long (rest (re-find re line)))]
                [[x1 y1] [x2 y2]])))
       (into {})))

;; How many sensors?
(count sensor->beacon)

;; How many beacons do we know about?
(->> sensor->beacon
     vals
     set
     count)

;; There are `34` sensors pointing to `8` beacons.

;; Which pairs have a radius that falls in `y=2000000`?
(def interesting-y 2000000)

;; Only consider sensor-beacon pairs that have a point that falls in the `y=2000000` axis.
;; This should cut down on doing some calculations.
(def qualified-sensor->beacon
  (->> sensor->beacon
       (filter (fn [[sensor beacon]]
                 (let [distance        (manhattan-distance sensor beacon)
                       sensor-y-origin (second sensor)
                       sensor-y-min    (- sensor-y-origin distance)
                       sensor-y-max    (+ sensor-y-origin distance)]
                   (or (<= sensor-y-origin interesting-y sensor-y-max)
                       (>= sensor-y-origin interesting-y sensor-y-min)))))))

;; How many sensor-beacon pairs qualified-sensor-beacon tell us about anything in row `y=2000000`?
(def min-sensor-x (->> qualified-sensor->beacon
                       (map (fn [[sensor beacon]]
                              (let [distance (manhattan-distance sensor beacon)
                                    [sensor-x _] sensor]
                                (- sensor-x distance))))
                       (apply min)))

(def max-sensor-x (->> qualified-sensor->beacon
                       (map (fn [[sensor beacon]]
                              (let [distance (manhattan-distance sensor beacon)
                                    [sensor-x _] sensor]
                                (+ sensor-x distance))))
                       (apply max)))

(def beacons
  (->> sensor->beacon
       vals
       set))

(defn in-range? [sensor beacon point]
  (let [distance-sensor-beacon (manhattan-distance sensor beacon)
        distance-sensor-point  (manhattan-distance sensor point)]
    (<= distance-sensor-point distance-sensor-beacon)))

;; Count each point and compare them to each potential sensor-beacon pair. If the point is not a
;; beacon and it falls within the range of at least one sensor-beacon pair then it can't be a beacon.
(transduce
  (comp
    (map (fn [x]
           [x interesting-y]))
    (map (fn [point]
           (if (and (not (beacons point))
                    (some (fn [[sensor beacon]]
                            (in-range? sensor beacon point))
                          qualified-sensor->beacon))
             1
             0))))
  +
  (range min-sensor-x (inc max-sensor-x)))

;; Done! 🎉🎉🎉

