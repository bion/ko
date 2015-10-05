(ns ko.gestures.single_signal
  [:use
   [ko.synth_defs.single_signal]
   [ko.gesture]
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

;; (deftype SinGliss [one two]
;;   IGesture
;;   (g-prepare [this])
;;   (g-start [this]))

(defn sin-gliss [name freq]
  {:g-start (play-event
             {:instr sin-synth
              :freq freq
              :amp 0.1
              :action 0})
   :g-kill #(kill (:synths %))
   :g-ctl (fn [k v] (ctl (:synths %) k v))})
