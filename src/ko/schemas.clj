(ns ko.schemas
  (:require [schema.core :as s]))

(def ScoreParseState
  "A schema for the parse-state entity `ko.score/parse-score`"
  {(s/required-key :beats-per-minute) s/Num
   (s/required-key :beats-per-bar) s/Num
   (s/required-key :jump-data) {(s/required-key :labels) s/Any

                                (s/required-key :jumps)
                                {s/Num {(s/required-key :label) s/Keyword
                                        (s/required-key :should-jump?) s/Any}}}

   (s/required-key :expanded-score) [s/Any]
   (s/required-key :curves) {s/Keyword [s/Any]}
   (s/required-key :score) [s/Any]
   (s/required-key :measure-num) s/Int
   (s/required-key :timestamp) s/Int})
