(ns aoc-2022.day05
  (:require [aoc-2022.util :as util]
            [nextjournal.clerk :as clerk]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

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
;;
;; I want to apply an instruction to my datastructure and have the return value
;; be the datastructure with that instruction applied. A map or a vector would work
;; and I can use `assoc`.

;; Something that looks like this: a vector where each element is a list that represents the stack.
(def sample-crates
  ['(\R \Q \G \P \C \F)
   '(\P \C \T \W)
   '(\C \M \P \H \B)
   '(\R \P \M \S \Q \T \L)])

;; A move from 1 to 2 would look like this
(def sample-out
  (with-out-str
    (pp/pprint
      (let [from  1
            to    2
            crate (peek (get sample-crates (dec from)))]
        (-> sample-crates
            (update (dec from) pop')
            (update
              (dec to)
              (fn [stack]
                (conj stack crate))))))))
(clerk/md
  (str
    "```\n"
    sample-out
    "\n```"))

;; The \R moved from the first stack to the second stack!

;; I can generalize the functionality above.
(defn move-crate [crates from to]
  (let [crate (peek (get crates (dec from)))]
    (if crate                                               ;; only do something if there's a crate
      (-> crates
          (update (dec from) pop')
          (update (dec to) (fn [stack]
                             (conj stack crate))))
      crates)))

;; # Parsing
;; The crates are a single letter and the number of columns are fixed so I can remove the unnecessary
;; characters. If I end up with a table of strings then I can transpose it to get a vector where each
;; element is the stack (as a list).
(def crates
  (->> input
       str/split-lines
       (take 8)
       (map (fn [line]
              (-> (str/replace line #"[\[\]]" "")
                  (str/replace #"  " " "))))
       (apply map vector) ;; transpose
       (map (fn [letters]
              ;; use into to convert the lazyseq returned from filter into a list
              (into '() (filter #(Character/isLetter %) letters))))
       (remove empty?)
       (map reverse)
       vec))

(defn pprint-md [x]
  (clerk/md
    (format
      "```\n%s\n```"
      (with-out-str (pp/pprint x)))))

;; Now we have a representation of our crates as a vector of lists 🎉
^{:nextjournal.clerk/visibility {:code :hide}}
(pprint-md crates)

;; Next, I need to parse the move instructions: `move 1 from 2 to 1`. I can use a regex.
(def move-instruction-re #"move (\d+) from (\d+) to (\d+)")

(re-find move-instruction-re "move 1 from 2 to 1")

(defn parse-move-instruction [instruction]
  (let [ns (drop 1 (re-find move-instruction-re instruction))]
    (map parse-long ns)))

(parse-move-instruction "move 1 from 2 to 1")

;; Now I need a function to execute the instruction on our stacks.
(defn apply-instruction [crates [amount from to]]
  (reduce (fn [crates _]
            (move-crate crates from to))
          crates
          (range amount)))

;; BEFORE
(pprint-md sample-crates)

;; AFTER, let's move 3 from 2 to 4
(pprint-md (apply-instruction sample-crates [3 2 4]))

;; Let's put it all together!
(def move-instructions
  (->> input
       str/split-lines
       (filter (fn [line]
              (str/starts-with? line "move")))
       (map parse-move-instruction)))

(def crates-final
  (reduce (fn [crates instruction]
            (apply-instruction crates instruction))
          crates
          move-instructions))

(pprint-md crates-final)

;; The crates on the top of the stack for Part 1 💪
(apply str (map peek crates-final))

;; # Part 2
;; ```
;; The CrateMover 9001 is notable for many new and exciting
;; features: air conditioning, leather seats, an extra cup holder,
;; and the ability to pick up and move multiple crates at once.
;; ```
;; For this part, I think the main thing is changing the way moving works so that you move
;; multiple crates at once instead of just one.

;; I should be able to modify the `move-crate` function to take in an argument
;; that specifies how many crates to move in one move.
(defn move-crate-2 [crates amount from to]
  (let [crates-to-move (take amount (get crates (dec from)))]
    (-> crates
        (update (dec from) (partial drop amount))
        (update (dec to) (fn [stack]
                           ;; regretting my decision to use a list as it gets weird
                           ;; when some functions return lazy seqs and now you have
                           ;; to keep converting them back to lists
                           (apply list (concat crates-to-move stack)))))))

(def crates-final-2
  (reduce (fn [crates [amount from to]]
            (move-crate-2 crates amount from to))
          crates
          move-instructions))

(pprint-md crates-final-2)

(apply str (map first crates-final-2))

;; Done! 🎉🎉
