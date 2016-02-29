(ns ko.gesture-test
  [:require [overtone.core :as ot]]
  (:use midje.sweet
        ko.synth-args))

(fact "`resolve-synth-args` resolves note keywords to HZ and negative numbers to amps"
      (resolve-synth-args [:freq :Eb4
                           :amp -12]) => [:freq 311.1269837220809
                                          :amp 0.251188643150958])
