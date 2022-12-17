(ns user
  (:require [nextjournal.clerk :as clerk]))

(comment

  ;; Clerk
  (require '[nextjournal.clerk :as clerk])
  (clerk/serve! {:browse? false})
  (clerk/halt!)
  (clerk/clear-cache!)



  (clerk/show! "src/day01.clj")


  )
