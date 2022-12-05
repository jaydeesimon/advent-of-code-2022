(ns aoc-2022.day05
  (:require [aoc-2022.util :as util]
            [nextjournal.clerk :as clerk]))

;; # Day 5

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day05.clj)

;; https://adventofcode.com/2022/day/5

(def input (util/fetch-input 5))

;; Let's print out the first 15 lines of the input to see what
;; it looks like.
(clerk/md (util/md-text-block input 15))

;; The representation of the crate configuration is in a
;; visual text format. I'll need to write something to parse
;; that into some kind of datastructure.


;; Then there are individual instructions that represent how to
;; operate on the crates. For example, `move 2 from 2 to 8`. I
;; can use a regex to parse the instruction lines.

;; I'm not sure how to parse that out but thinking ahead, the program needs to
;; 1. Parse the crate structure.
;; 2. The parsing of the crate structure will be the initial value of some datastructure representing the crates.
;; 3. Iterate and apply each instruction to the datastructure.
;; 4. The datastructure will be in the state that I can use to answer Part 1: `what crate ends up on top of each stack?`

;; # Datastructure of Crates
;; Since I'll need to simulate a stack, I can use a vector or a list as long as I understand
;; the right methods to call that will "pop" off the top of a stack and push onto the stack.

;; ## Using a List as a Stack
;; Here is a stack representing the first crate stack. The top is on the left and the bottom is on the right.
(def sample-stack (list "R" "Q" "G" "P" "C" "F"))

;; Remove the top of the stack and return the stack.
(pop sample-stack)

;; Push something onto the top of stack.
(conj sample-stack "NEW")

;; Look at the top of the stack.
(peek sample-stack)

;; This looks good except `pop` throws an exception if the list is empty.
(try
  (pop '())
  (catch Exception e
    (.getMessage e)))

;; I'll define my own version to check to see if it's empty first.
(defn pop' [coll]
  (if (empty? coll)
    coll
    (pop coll)))

;; Test it out.
(pop' '())
(pop' '(:single-item))

;; Now that I have some functions that will help with answering questions, the two big issues to tackle are
;; 1. Figure out my datastructure
;; 2. Parse the visual create representation
;;
;; Let's figure out the datastructure next...