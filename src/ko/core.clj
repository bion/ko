(ns ko.core
  [:use [overtone.core]]
  (:gen-class))

(defsynth sin-synth [amp 1 freq 200 outbus 0]
  (out outbus (* amp (sin-osc freq 0))))

(def nome (metronome 120))

(defn- quant-to-beat-in-bar [quant beat-zero]
  (+ beat-zero (- quant 1)))

(defn- play-event [event]
  ((:instr event)
   :freq (:freq event)
   :amp (:amp event)
   :env (:env event)))

(defn- schedule-measure [measure next-bar-beat]
  (doseq [[quant event] measure]
    (let [beat (quant-to-beat-in-bar quant next-bar-beat)]
      (at (nome beat) (play-event event)))))

(defn- schedule-cycle [measures]
  (let [next-measure (first measures)
        remaining-measures (rest measures)
        ;; measures are scheduled one beat before they begin
        next-bar-beat (metro-bar nome)
        next-cycle-timestamp (nome (+ 3 next-bar-beat))]

    (schedule-measure next-measure next-bar-beat)

    (apply-by next-cycle-timestamp
              #'schedule-cycle
              remaining-measures)))

(defn play-score [score]
  (nome :bpm (:bpm score))
  (schedule-cycle (:measures score)))
