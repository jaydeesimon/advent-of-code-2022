(ns aoc-2022.day13
  (:require [aoc-2022.util :as util]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; # Day 13

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day13.clj)

;; https://adventofcode.com/2022/day/13

(def input (util/fetch-input 13))

;; # Part 1

(def sample-pair "[[[[8,7,8,5,4],6,[4,6]]],[6,[[]]],[]]")

;; Should be able to convert the string straight to EDN.
(edn/read-string sample-pair)

;; Confirmed that each line is valid EDN. That's convenient.
(->> input
     str/split-lines
     (remove str/blank?)
     (map edn/read-string))

;; Pair each of them up.
(def pairs
  (->> input
       str/split-lines
       (remove str/blank?)
       (map edn/read-string)
       (partition 2)))

;; Use this function to switch on the type of `left` and `right` in our recursive
;; function.
(defn get-type [x]
  (cond
    (nil? x) nil
    (number? x) :number
    (vector? x) :vector
    :else (throw (ex-info "unknown type" {:type (type x)}))))

;; My functionality depends specifically on vectors so I'm using this
;; to a) make sure that any sequence returned becomes a vector. Also,
;; if it's the end of the `coll` return `nil` and not an empty seq which
;; is what would happen if `nil` was passed to `vec`.
(defn nextv [coll]
  (let [v (next coll)]
    (when v
      (vec v))))

;; I'll can do this recursively. Since what we do depends on the types of the left
;; and the right, I can list out every possibility for left and right and make sure
;; to cover each case.
(defn what-order [left right]
  (condp = [(get-type left) (get-type right)]
    
    ;; Got to the end of the list and no decision was made
    [nil nil]
    :keep-going
    
    ;; Two integers
    [:number :number]
    (cond
      (< left right) :in-the-right-order
      (> left right) :not-in-the-right-order
      :else :keep-going)
    
    ;; Only left side is an integer so retry it as a vector
    [:number :vector]
    (what-order [left] right)
    
    ;; Only right side is an integer so retry it as a vector
    [:vector :number]
    (what-order left [right])
    
    ;; Two vectors means compare element by element
    [:vector :vector]
    (loop [left*  left
           right* right]
      (let [the-order  (what-order (first left*) (first right*))
            next-left  (nextv left*)
            next-right (nextv right*)]
        (if (and (= :keep-going the-order)
                 ;; don't keep going if at the end of both lists
                 (not (and (nil? next-left) (nil? next-right))))
          (recur next-left next-right)
          the-order)))
    
    ;; Ran out of elements on the left
    [nil :number]
    :in-the-right-order
    
    ;; Ran out of elements on the left
    [nil :vector]
    :in-the-right-order
    
    ;; Ran out of elements on the right
    [:number nil]
    :not-in-the-right-order
    
    ;; Ran out of elements on the right
    [:vector nil]
    :not-in-the-right-order))

(let [pair  (first pairs)
      left  (first pair)
      right (second pair)]
  (what-order left right))

;; Some tests

(def sample-pairs
  (->> "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
       str/split-lines
       (remove str/blank?)
       (map edn/read-string)
       (partition 2)))

(->> sample-pairs
     (map-indexed (fn [idx [left right]]
                    {:pair-number (inc idx)
                     :order (what-order left right)}))
     (clerk/table))

;; This should add up to 13 ✅
(->> sample-pairs
     (map-indexed (fn [idx [left right]]
                    {:pair-number (inc idx)
                     :order (what-order left right)}))
     (filter #(= (:order %) :in-the-right-order))
     (map :pair-number)
     (reduce +))

;; I'll do it on the real input now.
(->> pairs
     (map-indexed (fn [idx [left right]]
                    {:pair-number (inc idx)
                     :order (what-order left right)}))
     (filter #(= (:order %) :in-the-right-order))
     (map :pair-number)
     (reduce +))

;; Done! 🥳
