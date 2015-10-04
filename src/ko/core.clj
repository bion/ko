(ns ko.core
  [:use
   [ko.scheduling]
   [ko.gestures.single_signal]]
  (:gen-class))

(def score
  {:bpm 120
   :bpb 3
   :measures [[1.0 [(sin-blip 440)]
               2.0 [(sin-blip 220)]
               3.0 [(sin-blip 220)]]

              [1.0 [(sin-blip 440)]
               2.0 [(sin-blip 220)]
               3.0 [(sin-blip 220)]]]})

(def changing-gesture
  {:bpm 160
   :bpb 4
   :measures [[1.0 [(sin-gliss :one 440)]
               2.0 [(adjust :one :freq 880)]
               3.0 [(adjust :one :freq 220)]
               4.0 [(adjust :one :freq 440)]]

              [1.0 [(finish :one)]]]})

;; TODO
;; instead of holding onto running synths, hold on to running procsses
;; processes will have their own means of cleaning things up, updating
;; when asked to instead of play-event, adjust, and finish directly
;; interacting with nodes

(play-score changing-gesture)
(ot/boot-server)
(play-score score)
(ot/stop)
