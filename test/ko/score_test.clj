(ns ko.score-test
  (:use midje.sweet
        ko.score
        ko.gesture
        ko.scheduling)
  (:require [overtone.core :as ot]))

(def test-parsed-score
  [{1 ['(begin :ssg :my-gesture-name {:instr test-synth, :freq 200})
       '(begin :ssg :other-gesture-name {:instr test-synth, :freq 400})]}])

(def test-mutations
  {:my-gesture-name
   [{:measure 1, :quant 1, :timestamp 0N,
     :spec [:spec {:instr 'test-synth, :freq 200}]}
    {:spec {:freq [220 :exp], :amp [0.1 :exp]}, :timestamp 10/9}]})

(facts "about `zip-mutations`"
       (fact
        (let [score test-parsed-score
              mutations test-mutations]
          (zip-mutations score mutations) =>
          [{1 ['(begin :ssg :my-gesture-name {:instr test-synth, :freq 200}
                       [{:measure 1, :quant 1, :timestamp 0N,
                         :spec [:spec {:instr test-synth, :freq 200}]}
                        {:spec {:freq [220 :exp], :amp [0.1 :exp]},
                         :timestamp 10/9}])
               '(begin :ssg :other-gesture-name {:instr test-synth, :freq 400})]}])))

(ko-defsynth test-synth
             [arg 1 arg-2 2]
             (ot/out 0 (ot/sin-osc:ar 220)))
(facts "about `defscore`"
       (fact
        (defscore test-score
          set-beats-per-bar 4
          set-beats-per-minute 108

          1 [(begin :ssg :my-gesture-name {:instr test-synth :freq 200})
             (begin :ssg :other-gesture-name {:instr test-synth :freq 400})]

          ;; specify envelope nodes
          3 [(! :my-gesture-name {:freq [220 :exp] :amp [0.1 :exp]})]) =>
        [[{1 [(begin :ssg :my-gesture-name {:instr test-synth :freq 200}
                     [{:timestamp 10/9 :spec {:freq [220 :exp] :amp [0.1 :exp]}}])
              (begin :ssg :other-gesture-name {:instr test-synth :freq 400})]}]]))

(comment
  (defscore my-score
    label :beginning
    set-beats-per-bar 4
    set-beats-per-minute 108

    1 [(begin :my-gesture-name {:type :ssg :spec {:freq 200}})
       (begin :other-gesture-name {:type :ssg :spec {:freq 400}})]

    ;; specify envelope nodes
    3 [(! {:name :my-gesture-name :spec {:freq [220 :exp] :amp [0.1 :exp]}})]

    1 [(begin {:type :ossg :spec {:freq 300 :dur 1}})]
    2 [(begin {:type :ossg :spec {:freq 400 :dur 1}})]
    3 [(begin {:type :ossg :spec {:freq 500 :dur 1}})]
    4 [(begin {:type :ossg :spec {:freq 600 :dur 1}})]

    set-beats-per-minute 150

    silent ;; one measure of silence
    silent-4 ;; four measures of silence

    1 [(finish :my-gesture-name :other-gesture-name)
       (begin :third-gesture-name {:type ssg :spec {:freq 500}})]

    1 [(begin {:type :ossg :spec {:freq 600 :dur 1}})]
    2 [(begin {:type :ossg :spec {:freq 500 :dur 1}})]
    3 [(begin {:type :ossg :spec {:freq 400 :dur 1}})]
    4 [(begin {:type :ossg :spec {:freq 300 :dur 1}})]

    1 [(jump-to :beginning (fn [jump-count] (< 1 jump-count)))
       (finish :third-gesture-name)]
    or
    1 [(jump-to :beginning #(continue-flag))
       (finish :third-gesture-name)]))
