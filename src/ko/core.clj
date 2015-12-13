(ns ko.core
  [:use
   [ko.scheduling]
   [ko.synth_defs.single_signal]
   [ko.gestures.single_signal]]
  (:gen-class))

(def score
  {:bpm 160
   :bpb 4
   :measures
   [[1.0 [[begin [ssg :one {:instr sin-synth
                            :freq 200
                            :amp 1
                            :action 0}]]]

     2.0 [[:shape :one :freq {:val 880 :curve :exp} {:amp 0.11}]]
     3.0 [[:shape :one :freq {:val 1200 :curve :lin} {:amp 0.25}]]
     4.0 [[adjust :one :amp 1]]]

    [1.0 [(finish :one)]]]})
