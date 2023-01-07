(ns aoc-2022.day15
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk])
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File)
           (java.awt.geom Rectangle2D Rectangle2D$Double Path2D$Double)))

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
#_(transduce
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

;; # Part 2

(def max-part2 4000000)

(defn point-in-any-sensor-range? [point]
  (some
    (fn [[sensor beacon]]
      (in-range? sensor beacon point))
    sensor->beacon))

;; This was a really bad idea.
(comment
  (transduce
    (comp
      (filter (fn [point]
                (and
                  (not (beacons point))
                  (not (point-in-any-sensor-range? point)))))
      (take 1))
    conj
    []
    (for [x (range 0 (inc max-part2))
          y (range 0 (inc max-part2))]
      [x y])))

(def min-x (->> sensor->beacon
                (mapcat (fn [[[sensor-x _] [beacon-x _]]]
                       [sensor-x beacon-x]))
                (apply min)))

(def max-x (->> sensor->beacon
                (mapcat (fn [[[sensor-x _] [beacon-x _]]]
                          [sensor-x beacon-x]))
                (apply max)))

(def min-y (->> sensor->beacon
                (mapcat (fn [[[_ sensor-y] [_ beacon-y]]]
                          [sensor-y beacon-y]))
                (apply min)))

(def max-y (->> sensor->beacon
                (mapcat (fn [[[_ sensor-y] [_ beacon-y]]]
                          [sensor-y beacon-y]))
                (apply max)))

(def svg-view-box
  (let [width (- max-x min-x)
        height (- max-y min-y)]
    (format "%d %d %d %d" min-x min-y width height)))

(defn plot-sensor-beacon [[sensor-x sensor-y] [beacon-x beacon-y]]
  (let [distance (manhattan-distance [sensor-x sensor-y] [beacon-x beacon-y])
        p1       (format "%d,%d" (- sensor-x distance) sensor-y)
        p2       (format "%d,%d" sensor-x (- sensor-y distance))
        p3       (format "%d,%d" (+ sensor-x distance) sensor-y)
        p4       (format "%d,%d" sensor-x (+ sensor-y distance))]
    [:polygon {:points (str/join " " [p1 p2 p3 p4]) :fill "blue"}]))

(def sample-beacon-sensor1 (first sensor->beacon))
(def sample-beacon-sensor2 (second sensor->beacon))

(last
  (take 3 sensor->beacon))

(clerk/html #_{::clerk/width :full}
  [:svg {:viewBox svg-view-box}
   #_[:rect {:x min-x :y min-y :width (- max-x min-x) :height (- max-y min-y) :fill "green"}]
   [:rect {:x 0 :y 0 :width "4000000" :height "4000000" :fill "yellow"}]
   (map
     (fn [[sensor beacon]]
       (plot-sensor-beacon sensor beacon))
     #_[[1886537 2659379] [2810772 2699609]]
     sensor->beacon)])