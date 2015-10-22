(ns ko.score-test
  (:use midje.sweet)
  (:require [ko.score :refer :all]))

(facts "about `extract-next-measure`"
       (fact
        (extract-next-measure
         '(1 [(+ 1 1)])) => [[1 ['(+ 1 1)]] '()]))

(facts "about `read-score`"
       (fact
        (read-score) => [])

       (fact
        (read-score
         1 [(+ 2 2)]) => [[1 [4]]])

       (fact
        (read-score
         1 [(+ 1 1)
            2]

         2 [3]

         ;; 3 [(! {:name (:name a-gesture) :spec {:freq 220}})]

         1 [3]) => [ [1 [2 2]
                      2 [3]]
                     [1 [3]] ])

       (fact
        (read-score
         1 [(+ 1 1)
            2]
         silent) => [ [1 [2 2]]
                      [0 []] ] ))

(comment
  (read-score
   label :beginning
   set-beats-per-bar 4
   set-beats-per-minute 108

   1 [(begin :other-gesture-name (ssg {:freq 200}))
      ((begin :other-gesture-name (ssg {:freq 400})))]
   3 [(! {:name :my-gesture-name :spec {:freq 220}})]

   1 [(begin (ossg {:freq 300 :dur 1}))]
   2 [(begin (ossg {:freq 400 :dur 1}))]
   3 [(begin (ossg {:freq 500 :dur 1}))]
   4 [(begin (ossg {:freq 600 :dur 1}))]

   set-beats-per-minute 150

   silent ;; one measure of silence
   silent-4 ;; four measures of silence

   1 [(finish :my-gesture-name :other-gesture-name)
      (begin :third-gesture-name (ssg {:freq 500}))]

   1 [(begin (ossg {:freq 600 :dur 1}))]
   2 [(begin (ossg {:freq 500 :dur 1}))]
   3 [(begin (ossg {:freq 400 :dur 1}))]
   4 [(begin (ossg {:freq 300 :dur 1}))]

   1 [(jump-to :beginning (fn [jump-count] (< 1 jump-count)))
      (finish :third-gesture-name)]
   or
   1 [(jump-to :beginning continue-flag)
      (finish :third-gesture-name)]
   ))
