(ns ko.gesture-test
  [:require [overtone.core :as ot]]
  (:use midje.sweet
        ko.gesture))

(facts "about `ko-defsynth`"
       (against-background
        [ (before :facts (reset! ko-synth-templates {})) ]
        (fact "it adds the body of the synth to the ko-synth-templates atom"
         (do
           (ko-defsynth test-synth
                        [one 1 two 2]
                        (ot/out 0 0))
           @ko-synth-templates) => {:test-synth '([one 1 two 2]
                                                  (ot/out 0 0))})))
