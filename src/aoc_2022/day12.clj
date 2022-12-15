(ns aoc-2022.day12
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

;; # Day 12

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day12.clj)

;; https://adventofcode.com/2022/day/12

(def input (util/fetch-input 12))

;; # Part 1

;; >What is the fewest steps required to move from your current position to
;; the location that should get the best signal?

;; How many rows?
(def rows (->> input
               str/split-lines
               count))

;; How many columns?
(def cols
  (->> input
       str/split-lines
       first
       count))

;; I can only move up, down, left and right.
(def directions
  [[-1 0] [0 -1] [0 1] [1 0]])

;; Define a map where the key is the coordinate and the value is the letter. I
;; can use this to look up valid coordinates when trying to define a graph.
(def coordinate->letter
  (->> input
       str/split-lines
       (mapcat (fn [row line]
                 (map (fn [col letter]
                        [[row col] letter])
                      (range) line))
               (range))
       (into {})))

;; Given a coordinate, return the valid coordinates surrounding this coordinate.
(defn surrounding-coordinates [coordinate]
  (->> directions
       (map (fn [direction]
                 (mapv + direction coordinate)))
       ;; only include coordinates that exist in our map
       (filter coordinate->letter)))

;;>To avoid needing to get out your climbing gear, the elevation of the
;; destination square can be at most one higher than the elevation of your
;; current square; that is, if your current elevation is m, you could step
;; to elevation n, but not to elevation o. (This also means that the elevation
;; of the destination square can be much lower than the elevation of your current
;; square.)

;; To paraphrase the above, a coordinate is reachable if it's only one step higher,
;; the same height or lower. Take into account the start (`a`) and end (`z`) values.
(defn reachable? [current-letter dest-letter]
  (let [aliases        {\S \a \E \z}
        current-letter (or (aliases current-letter) current-letter)
        dest-letter    (or (aliases dest-letter) dest-letter)
        elevation-diff (- (int dest-letter) (int current-letter))]
    (<= elevation-diff 1)))

;; Define the edges and put them in [src dest] format so I can load them up into
;; ubergraph.
(def edges
  (->> (for [x (range rows)
             y (range cols)]
         [x y])
       (mapcat (fn [coordinate]
                 (let [letter                  (coordinate->letter coordinate)
                       surrounding-coordinates (surrounding-coordinates coordinate)]
                   (->> surrounding-coordinates
                        (filter (fn [surrounding-coordinate]
                                  (let [dest-letter (coordinate->letter surrounding-coordinate)]
                                    (reachable? letter dest-letter))))
                        (map (fn [surrounding-coordinate]
                               [coordinate surrounding-coordinate]))))))))

;; 👋 Hello [ubergraph!](https://github.com/Engelberg/ubergraph)
(def graph
  (apply uber/digraph edges))

;; Fine the starting coordinate!
(def start (->> coordinate->letter
                (filter (fn [[k v]]
                          (= v \S)))
                ffirst))
;; Fine the ending coordinate!
(def end (->> coordinate->letter
              (filter (fn [[k v]]
                        (= v \E)))
              ffirst))

;; Let's count the number of nodes in our path and subtract one because I
;; don't need to count the starting node.
(dec
  (count
    (alg/nodes-in-path
      (alg/shortest-path graph {:start-node start, :end-node end}))))

;; Sweet! 🎉🎉

;; # Part 2
;; >What is the fewest steps required to move starting from any
;; square with elevation a to the location that should get the best signal?

;; First I need to find all of the possible starting points.
(def starts (->> coordinate->letter
                (filter (fn [[k v]]
                          (or (= v \S) (= v \a))))
                 (map first)))

;; For each starting point, find the shortest path and then take the smallest value.
(->> starts
     (map (fn [start]
            (-> (alg/shortest-path graph start end)
                alg/nodes-in-path
                count
                dec)))
     (filter pos?)
     sort
     first)

;; Cool! 🎉🎉🎉🎉
