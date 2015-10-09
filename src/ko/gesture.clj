(ns ko.gesture
  [:use [overtone.core]]
  (:gen-class))

(defonce ^{:doc "Protect gesture protocol against namespace reloads"}
  _GESTURE_PROTOCOL_
  (do
    (defprotocol IGesture
      (g-nodes [this] "Returns a vector of synth-ids the gesture is responsibile for.")
      (get-g-name [this] "Returns name of gesture")
      (g-prepare [this] [this spec] "Perform any preperation needed for the gesture.")
      (g-start [this] [this spec] "Start playing the gesture.")
      (g-ctl [this spec] "Control the gesture while it's playing.")
      (g-end [this] [this time] "Kill related synths and clean up the gesture."))))
