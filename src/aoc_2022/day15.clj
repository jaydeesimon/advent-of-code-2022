(ns aoc-2022.day15
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

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

;; # Part 2

;;>Find the only possible position for the distress beacon. What is its tuning frequency?

;; Took me a while to figure out Part 2. I initially got tripped up by the sensor-beacon
;; boxes being turned at 45 degrees. Eventually, I realized I can think about the boxes
;; as groups of vertical lines representing y ranges. At that point, it was a matter of
;; figuring out how to calculate the y ranges and merging them.

;; Ugh took forever though.

(def max-part2 4000000)

;; Standard way to merge ranges aka intervals.
(defn merge-ranges [ranges]
  (if (not (seq ranges))
    ranges
    (let [ranges (sort-by first ranges)]
      (loop [stack  [(first ranges)]
             ranges (rest ranges)]
        (if (not (seq ranges))
          stack
          (let [[top-start top-end] (peek stack)
                current (first ranges)
                [current-start current-end] current]
            (if (< top-end current-start)
              (recur (conj stack current) (rest ranges))
              (let [top-end (max top-end current-end)]
                (recur (conj (pop stack) [top-start top-end]) (rest ranges))))))))))

;; Given a sensor-beacon box and an `x`, calculate the y range at that `x`.
(defn y-range [sensor beacon x]
  (let [radius (manhattan-distance sensor beacon)
        [sensor-x sensor-y] sensor
        center-x sensor-x
        offset (- radius (abs (- center-x x)))]
    [(- sensor-y offset) (+ sensor-y offset)]))

;; Once I have a list of merged ranges, this function can be used to find any open gaps.
;; The open gap (should just be of size one) is where the distress beacon lies.
(defn find-gaps [ranges]
  (->> ranges
       (partition 2 1)
       (map (fn [[[_ r1-end] [r2-start _]]]
              [(inc r1-end) (dec r2-start)]))
       (filter (fn [[start end]]
                 (<= start end)))))

(defn distress-beacon-range-possibilities [x]
  (->> sensor->beacon
       (map (fn [[sensor beacon]]
              (y-range sensor beacon x)))
       (filter (fn [[start end]]
                 (<= start end)))
       merge-ranges
       find-gaps))

;; Loop through every `x` in the bounding box, calculate the `y` ranges where the distress 
;; CANNOT be and then look for a gap in those ranges.
(transduce
  (comp
    (map (fn [x]
           {:x x :gaps (distress-beacon-range-possibilities x)}))
    (filter (comp seq :gaps)) ;; keep elements where there is a gap
    (take 1))
  conj
  []
  (range 0 (inc max-part2)))

(defn tuning-frequency [x y]
  (+ (* max-part2 x) y))

(tuning-frequency 3435885 2639657)

;; Done! 🎉🎉🎉
