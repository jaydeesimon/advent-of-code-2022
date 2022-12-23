(ns aoc-2022.garbage)


(comment

  ;; Day 14 Garbage

  (def floor-y (->> initial-grid
                    keys
                    (map second)
                    (apply max)
                    inc
                    inc))

  (def sample-floor-y (->> initial-sample-grid
                           keys
                           (map second)
                           (apply max)
                           inc
                           inc))

  (defn next-move-2 [grid current floor-y]
    (or
      (some
        (fn [possible-move]
          (when (and (nil? (grid possible-move))
                     (not= floor-y (second possible-move)))
            possible-move))
        (possible-moves current))
      current))

  (defn advance-state-2 [grid floor-y]
    (let [sands (find-sands grid)]
      (reduce (fn [grid sand-position]
                (let [next-move (next-move-2 grid sand-position floor-y)]
                  (if (not= next-move sand-position)
                    (-> (assoc grid next-move :sand)
                        (dissoc sand-position))
                    (assoc grid sand-position :sand-at-rest))))
              grid
              sands)))

  ;; What does the grid look like with the floor?
  ^{:nextjournal.clerk/width :full}
  (clerk/md
    (format "```\n%s\n```" (grid->string initial-grid floor-y)))

  (def final-grid-2
    (loop [grid0 initial-grid
           grid  (advance-state-2 grid0 floor-y)
           c     1]
      (when (zero? (mod c 10000))
        (println c "iterations part 2"))
      (cond
        ;; use this to break out early so I can
        ;; verify its working
        (> c 800000)
        grid

        ;; sand is covering the source
        (= (get grid [500 0]) :sand-at-rest)
        grid

        ;; grid is at rest, add in new sand
        (= grid0 grid)
        (let [grid (assoc grid [500 0] :sand)]
          (recur grid (advance-state-2 grid floor-y) (inc c)))

        ;; move on to the next state
        :else
        (recur grid (advance-state-2 grid floor-y) (inc c)))))

  ;; What does the grid look like all filled up?
  ^{:nextjournal.clerk/width :full}
  (clerk/md
    (format "```\n%s\n```" (grid->string final-grid-2 floor-y)))

  (->> (vals final-grid-2)
       (filter #(= % :sand-at-rest))
       count)

  (count final-grid-2)


  )
