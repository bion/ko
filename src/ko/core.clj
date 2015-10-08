(ns ko.core
  [:use
   [ko.scheduling]
   [ko.synth_defs.single_signal]
   [ko.gestures.single_signal]]
  (:gen-class))

(def changing-gesture
  {:bpm 160
   :bpb 4
   :measures [[1.0 [(begin (ssg :one {:instr sin-synth :freq 200 :amp 1 :action 0}))]
               2.0 [(adjust :one :freq 880 :amp 0.1)]
               3.0 [(adjust :one :freq 220 :amp 0.25)]
               4.0 [(adjust :one :freq 440 :amp 1)]]
              [1.0 [(finish :one)]]]})

(play-score changing-gesture)
(overtone.core/stop)
(overtone.core/boot-server)
