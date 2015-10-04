(ns ko.gestures.single_signal
  [:use
   [ko.synth_defs.single_signal]
   [ko.scheduling]]
  (:gen-class))

(defn sin-blip [freq]
  #(play-event {:instr sin-synth
                :freq freq
                :amp 0.1}))

(defn sin-gliss [name freq]
  #(swap! running-synths
          assoc name (play-event
                      {:instr sin-synth
                       :freq freq
                       :amp 0.1
                       :action 0})))
