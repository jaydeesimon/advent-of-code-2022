(ns aoc-2022.day07
  (:require [aoc-2022.util :as util]
            [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; # Day 7

;; Source available on [GitHub.](https://github.com/jaydeesimon/advent-of-code-2022/blob/main/src/aoc_2022/day07.clj)

;; https://adventofcode.com/2022/day/7

(def input (util/fetch-input 7))

;; # Part 1

;; >Find all of the directories with a total size of at most 100000.
;; What is the sum of the total sizes of those directories?

;; The "at most 100000" read a little confusing to me but I think it means
;; less than or equal to 100000.

;; The input represents a tree, sorta. Taking a look at the input:
(clerk/md
  (util/md-text-block input 50))

;; There is a tree in there somewhere and so the question is,
;; what's the best way to reconstruct that tree such that I'll
;; be able to ask the question of Part 1?

;; In a way, this output represents a flattened tree.

;; Looking closer at the output, the commands are navigating
;; the filesystem (tree). This might be a good reason to use
;; zippers. Can I navigate and construct the tree at the same time?


;; ## Approach

;; I can reduce over each line and pay attention to only the `cd`s (change directories)
;; and the files. Whenever a directory is changed into, the files are listed.
;; I can use this to keep track of the level. When I get to a file, I can update the
;; size of the directories at each level.
(defn update-size-at-each-level [tree level filesize]
  (loop [tree tree
         level level]
    (if (not (seq level))
      tree
      (recur (update-in tree (conj level :size) (fn [size]
                                                  (+ (or size 0) filesize)))
             (vec (butlast level))))))

(def filesystem
  (first
    (reduce (fn [[tree level] line]
              (cond
                ;; go up one level
                (str/starts-with? line "$ cd ..")
                [tree (vec (butlast level))]
                
                ;; go down one level into the named dir
                (str/starts-with? line "$ cd")
                (let [re    #"\$ cd (.+)"
                      [_ dirname] (re-find re line)
                      level (conj level dirname)]
                  [(assoc-in tree level {}) level])
                
                ;; if it's a file, update the sizes at each level
                (Character/isDigit (first line))
                (let [[_ filesize] (re-find #"(\d+)" line)
                      filesize (parse-long filesize)]
                  [(update-size-at-each-level tree level filesize)
                   level])
                
                ;; ignore everything else
                :else
                [tree level]))
            [{} []]
            (str/split-lines input))))

;; Now I can use a function to traverse the tree and get each path. Each path represents
;; a directory's size. 
(defn paths [m]
  (letfn [(paths* [ps ks m]
            (reduce-kv
              (fn [ps k v]
                (if (map? v)
                  (paths* ps (conj ks k) v)
                  (conj ps (conj ks k))))
              ps
              m))]
    (paths* () [] m)))

;; Here is a sample path.
(first (paths filesystem))

;; And if I get the value at the path, I'll have the size.
(get-in filesystem (first (paths filesystem)))

;; Now let's find all of the sizes that are less than 100,000
;; and then add them up!
(->> (paths filesystem)
     (map (fn [path]
            (get-in filesystem path)))
     (filter (fn [size]
               (<= size 100000)))
     (reduce +))

;; Hooray! 🎉🎉

;; # Part 2
;; >The total disk space available to the filesystem is 70000000.
;; To run the update, you need unused space of at least 30000000.
;; You need to find a directory you can delete that will free up
;; enough space to run the update.

;; Total used disk space:
(get-in filesystem ["/" :size])

;; Percentage of used disk space:
(* (/ (get-in filesystem ["/" :size]) 70000000.) 100)

;; Need this much space:
(def space-needed
  (- 30000000 (- 70000000 (get-in filesystem ["/" :size]))))

;; Find all the directories that, if deleted, would give us at least that much space.
(def directory-size-choices
  (->> (paths filesystem)
       (map (fn [path]
              (get-in filesystem path)))
       (filter (fn [size]
                 (>= size space-needed)))))

;; Take the smallest one:
(apply min directory-size-choices) 

;; Done! 🎉🎉
