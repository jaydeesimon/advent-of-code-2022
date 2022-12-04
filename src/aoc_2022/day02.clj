(ns aoc-2022.day02
  (:require [aoc-2022.util :as util]
            [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; https://adventofcode.com/2022/day/2

;; # Day 2
(def input (util/fetch-input 2))

(def move-scores
  {:rock 1
   :paper 2
   :scissors 3})

(def letter->move
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def winner
  {#{:paper :rock} :paper
   #{:scissors :rock} :rock
   #{:paper :scissors} :scissors})

(defn score [[left right]]
  (let [left-move  (letter->move left)
        right-move (letter->move right)]
    (if (= left-move right-move)
      (+ 3 (move-scores right-move))
      (let [winner* (winner #{left-move right-move})]
        (if (= winner* left-move)
          (move-scores right-move)
          (+ 6 (move-scores right-move)))))))

(def moves
  (->> input
       str/split-lines
       (map (fn [line]
              (let [[left right] (str/split line #" ")]
                [left right])))))

;; # Part 1
;; Calculate the scores per line and sum them up.
(reduce + (map score moves))

(def letter->outcome
  {"X" :lose
   "Y" :draw
   "Z" :win})

(def move-choice
  {[:rock :lose] :scissors
   [:rock :win] :paper
   [:paper :lose] :rock
   [:paper :win] :scissors
   [:scissors :lose] :paper
   [:scissors :win] :rock})

(defn score-2 [[left right]]
  (let [left-move (letter->move left)
        outcome (letter->outcome right)]
    (if (= outcome :draw)
      (+ 3 (move-scores left-move))
      (let [right-move (move-choice [left-move outcome])]
        (if (= outcome :lose)
          (move-scores right-move)
          (+ 6 (move-scores right-move)))))))

;; # Part 2
;; What are the scores if we had to choose based on a desired outcome?
(reduce + (map score-2 moves))


