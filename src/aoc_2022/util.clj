(ns aoc-2022.util
  (:require [clj-http.client :as http])
  (:import (java.io File)))

(defn fetch-input [day]
  (let [input-filename (str "inputs/" day ".txt")]
    (if (.exists (File. input-filename))
      (slurp input-filename)
      (let [url   (format "https://adventofcode.com/2022/day/%d/input" day)
            input (:body (http/get url {:headers {"Cookie" (slurp ".cookie")}}))]
        (spit input-filename input)
        input))))




(comment

  (fetch-input 1)



  )