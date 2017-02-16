(ns ko.synths
  [:use
   [overtone.core]
   [ko.gesture]])

(ko-defsynth
 metronome-beep->
 [outbus 0 freq 1800 amp 1]
 (let [env (env-gen:kr (env-perc 0.001 0.05 amp -4) :action 2)
       sig (sin-osc:ar freq)]
   (out:ar outbus (* env sig))))
