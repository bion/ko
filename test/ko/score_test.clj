(ns ko.score-test
  (:use midje.sweet
        ko.score
        ko.gesture
        ko.scheduling)
  (:require [overtone.core :as ot]))

(if-not (ot/server-connected?)
  (ot/boot-server))

(ko-defsynth test-synth
             [arg 1 arg-2 2]
             (ot/out 0 (ot/sin-osc:ar 220)))

(def test-parsed-score
  [{1 [(begin :ssg :my-gesture-name {:instr test-synth, :freq 200})
       (begin :ssg :other-gesture-name {:instr test-synth, :freq 400})]}])

(def test-mutations
  {:my-gesture-name
   [{:measure 1, :quant 1, :timestamp 0N,
     :spec [:spec {:instr test-synth, :freq 200}]}
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
               '(begin :ssg :other-gesture-name {:instr test-synth, :freq 400} [])]}])))

(facts "about `filter-empty-mutations`"
       (fact (let [mutations {:one [1 2 3] :two [1] :three [1 2]}]
               (filter-empty-mutations mutations) => {:one [1 2 3] :three [1 2]})))

(facts "about `parse-score`"
       (let [[actions mutations jumps]
             (parse-score
              '(beats-per-bar 4
                beats-per-minute 108

                label :one
                1 [(begin :ssg :my-gesture-name {:instr test-synth :freq 200})
                   (begin :ssg :other-gesture-name {:instr test-synth :freq 400})]

                jump-to :one
                3 [(mutate :my-gesture-name {:freq [220 :exp] :amp [0.1 :exp]})]))
             expected-actions [{1
                                 [(begin :ssg :other-gesture-name
                                         {:freq 400, :instr test-synth})
                                  (begin :ssg :my-gesture-name
                                         {:freq 200, :instr test-synth})]}]
             expected-mutations {:my-gesture-name
                                  [{:measure 1,
                                    :quant 1,
                                    :spec {:freq 200, :instr test-synth},
                                    :timestamp 0N}
                                   {:measure 2,
                                    :quant 3,
                                    :spec {:amp [0.1 :exp], :freq [220 :exp]},
                                    :timestamp 10/3}],
                                  :other-gesture-name
                                  [{:measure 1,
                                    :quant 1,
                                    :spec {:freq 400, :instr test-synth},
                                    :timestamp 0N}]}
             expected-jumps (just
                             {:jumps
                              (just {1 (just {:label :one :should-jump? fn?})})
                              :labels
                              {:one 0}})]
         (fact actions => expected-actions)
         (fact mutations => expected-mutations)
         (fact jumps => expected-jumps)))

(facts "about `true-for-n`"
       (let [instance (true-for-n 2)]
         (fact "returns true for the number of times passed, then false"
               (do (instance) => true
                   (instance) => true
                   (instance) => false))))
