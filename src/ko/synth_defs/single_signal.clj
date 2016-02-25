(ns ko.synth_defs.single_signal
  [:use [overtone.core]])

(defsynth sin-synth [amp 1 freq 440 outbus 0 action 2]
  (out outbus
       (* amp
          (sin-osc freq)
          (env-gen:kr
           (envelope [0 1 1]
                     [0.01 0.3]
                     [-1 -1])
           :action action))))
