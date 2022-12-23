(ns aoc-2022.day14p1
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; # Day 14

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day14p1.clj)

;; https://adventofcode.com/2022/day/14

(def input (util/fetch-input 14))

;; # Part 1
;; >Using your scan, simulate the falling sand. How many units of sand come to rest before sand starts flowing into
;; the abyss below?

(def down [0 1])
(def down-left [-1 1])
(def down-right [1 1])

(def possible-directions [down down-left down-right])

(defn possible-moves [from]
  (map (fn [possible-direction]
         (mapv + possible-direction from))
       possible-directions))

(defn next-move [grid current]
  (or
    (some
      (fn [possible-move]
        (when (nil? (grid possible-move))
          possible-move))
      (possible-moves current))
    current))

(possible-moves [0 0])

;; If the sand can't move down, down-left or down-right, return the current position
;; which means it stays at rest.
(let [current [0 0]]
  (next-move {[0 1] :rock
              [-1 1] :sand
              [1 1] :rock}
             current))

;; Given a magnitude, get the unit value.
(defn unit-value [magnitude]
  (cond
    (pos? magnitude) 1
    (neg? magnitude) -1
    :else 0))

(unit-value -0)

;; 544,163 -> 544,161
(->> (mapv - [544 161] [544 163])
     (mapv unit-value))

(defn points-in-line [start end]
  (let [distance (mapv - end start)
        direction (mapv unit-value distance)]
    (loop [points #{}
           current start]
      (if (= current end)
        (conj points current)
        (recur (conj points current)
               (mapv + current direction))))))

(points-in-line [544 163] [544 161])

(sort-by first (points-in-line [507 100] [517 100]))

(defn sand-comparator [[x1 y1] [x2 y2]]
  (if (= y1 y2)
    (compare x1 x2)
    (compare y2 y1)))

(defn find-sands [grid]
  (->> grid
       (filter (fn [[position type]]
                 (= type :sand)))
       (map first)))

(find-sands {[0 0] :sand
             [0 3] :sand
             [-1 3]:sand
             [0 1] :rock
             [-5 4] :sand})

(defn advance-state [grid]
  (let [sands (find-sands grid)]
    (reduce (fn [grid sand-position]
              (let [next-move (next-move grid sand-position)]
                (if (not= next-move sand-position)
                  (-> (assoc grid next-move :sand)
                      (dissoc sand-position))
                  (assoc grid sand-position :sand-at-rest))))
            grid
            sands)))

(let [grid {[0 0] :sand}]
  (advance-state grid))

(let [grid {[0 0] :sand
            [0 1] :rock}]
  (advance-state grid))

(let [grid {[0 0] :sand
            [0 1] :rock
            [-1 1] :rock}]
  (advance-state grid))

(let [grid {[0 0] :sand
            [0 1] :rock
            [-1 1] :rock
            [1 1] :rock}]
  (advance-state grid))

(defn find-lowest-type [grid type]
  (let [found-types (->> grid
                         (filter (fn [[position type*]]
                                   (= type* type))))]
    (when (seq found-types)
      (->> (map first found-types)
           (reduce (fn [acc position]
                     (max-key second acc position)))))))

(defn sand-falls-into-abyss? [grid]
  (let [[_ rock-y] (find-lowest-type grid :rock)
        [_ sand-y] (find-lowest-type grid :sand)]
    (when (and sand-y rock-y)
      (>= sand-y rock-y))))

(defn initialize-grid [lines]
  (reduce (fn [grid [p1 p2]]
            (reduce (fn [grid* p*]
                      (assoc grid* p* :rock))
                    grid
                    (points-in-line p1 p2)))
          {}
          lines))

(def initial-sample-grid
  (let [lines [[[498 4] [498 6]]
               [[498 6] [496 6]]
               [[503 4] [502 4]]
               [[502 4] [502 9]]
               [[502 9] [494 9]]]]
    (initialize-grid lines)))

(def final-sample-grid
  (loop [grid0 initial-sample-grid
         grid (advance-state grid0)]
    (cond
      ;; grid is at rest, add in new sand
      (= grid0 grid)
      (let [grid (assoc grid [500 0] :sand)]
        (recur grid (advance-state grid)))
      
      ;; sand has fallen into the abyss, all done
      (sand-falls-into-abyss? grid)
      grid
      
      ;; move on to the next state    
      :else
      (recur grid (advance-state grid)))))

;; Count the sand and subtract the one falling into the abyss.
(->> final-sample-grid
     vals
     (filter #(= % :sand))
     count
     dec)

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

(def initial-grid (initialize-grid lines))

(find-lowest-type initial-grid :rock)

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
                          (= e :sand) \+
                          (= e :sand-at-rest) \o
                          :else \.)]]
            c)
          (partition partition-size)
          (map #(apply str %))
          (str/join "\n")))))

(def final-grid
  (loop [grid0 initial-grid
         grid  (advance-state grid0)
         c     1]
    (cond
      ;; use this to break out early so I can
      ;; verify its working
      (> c 100000)
      grid

      ;; grid is at rest, add in new sand
      (= grid0 grid)
      (let [grid (assoc grid [500 0] :sand)]
        (recur grid (advance-state grid) (inc c)))

      ;; sand has fallen into the abyss, all done
      (sand-falls-into-abyss? grid)
      grid

      ;; move on to the next state
      :else
      (recur grid (advance-state grid) (inc c)))))

;; Print the map.
^{:nextjournal.clerk/width :full}
(clerk/md
  (format "```\n%s\n```" (grid->string final-grid)))

;; How many sands are at rest?
(->> (vals final-grid)
     (filter #(= % :sand-at-rest))
     count)

;; That was really messy but done with Part 1! 🎉🎉

;; # Part 2

;; >Using your scan, simulate the falling sand until the source of the sand becomes blocked.
;; How many units of sand come to rest?

;;>You don't have time to scan the floor, so assume the floor is an infinite horizontal line with a y
;; coordinate equal to two plus the highest y coordinate of any point in your scan.

;; Going to do Part 2 in a separate namespace because I want to start over...[click here.](https://jaydeesimon.github.io/advent-of-code-2022/src/aoc_2022/day14p2.html)

