(ns ko.core
  [:use [overtone.core]]
  (:gen-class))

(defsynth sin-synth [amp 1 freq 440 outbus 0]
  (out outbus (* amp
                 (sin-osc freq)
                 (env-gen:kr (envelope [0 1 0] [0.01 0.3] [-1 -1]) :action 2))))

(def nome (metronome 120))

(defn- quant-to-beat-in-bar [quant beat-zero]
  (+ beat-zero (- quant 1)))

(defn- play-event [event]
  ((:instr event)
   :freq (:freq event)
   :amp (:amp event)))

(defn- schedule-measure [measure next-bar-beat]
  (doseq [[quant events] (partition 2 measure)]
    (let [beat (quant-to-beat-in-bar quant next-bar-beat)]
      (at (nome beat)
          (doseq [event events]
            (play-event event))))))

(defn- schedule-cycle [measures]
  (let [next-measure (first measures)
        remaining-measures (rest measures)
        ;; measures are scheduled one beat before they begin
        next-bar-beat (* 4 (metro-bar nome))
        next-cycle-timestamp (nome (+ 3 next-bar-beat))]

    (schedule-measure next-measure next-bar-beat)

    (apply-by next-cycle-timestamp
              #'schedule-cycle
              [remaining-measures])))

(defn play-score [score]
  (nome :bpm (:bpm score))
  (schedule-cycle (:measures score)))

(defn sin-blip [freq]
  {:instr sin-synth
   :freq freq
   :amp 1
   :outbus 0})

(def score
  {:bpm 116
   :measures [[1.0 [(sin-blip 440)]
               2.0 [(sin-blip 220)]
               3.0 [(sin-blip 220)]
               4.0 [(sin-blip 220)]]

              [1.0 [(sin-blip 440)]
               2.0 [(sin-blip 220)]
               3.0 [(sin-blip 220)]
               4.0 [(sin-blip 220)]]
              ]})

(play-score score)
