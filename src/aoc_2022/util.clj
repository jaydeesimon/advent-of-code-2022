(ns aoc-2022.util
  (:require [clj-http.client :as http]
            [clojure.string :as str])
  (:import (java.io File)))

(defn fetch-input [day]
  (let [input-filename (str "inputs/" day ".txt")]
    (if (.exists (File. input-filename))
      (slurp input-filename)
      (let [url   (format "https://adventofcode.com/2022/day/%d/input" day)
            input (:body (http/get url {:headers {"Cookie" (slurp ".cookie")}}))]
        (spit input-filename input)
        input))))

(defn md-text-block [input n]
  (let [text (->> input
                  str/split-lines
                  (take n)
                  (map (fn [line]
                         ;; to get it to render a blank newline,
                         ;; you need to replace it with spaces
                         (if (str/blank? line)
                           "    "
                           line)))
                  (str/join "\n"))]
    (format "```\n%s\n```" text)))


(comment

  (fetch-input 1)



  )