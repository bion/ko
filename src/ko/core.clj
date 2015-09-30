(ns ko.core
  [:use [overtone.core]]
  (:gen-class))

(defsynth sin-synth [amp 1 freq 440 outbus 0]
  (out outbus
       (* amp
          (sin-osc freq)
          (env-gen:kr
           (envelope [0 1 0]
                     [0.01 0.3]
                     [-1 -1])
           :action 2))))

(def ^:dynamic beats-per-bar 4)
(def ^:dynamic beats-per-minute 120)

(defn calc-beat-dur []
  (/ 60.0 beats-per-minute))

(defn calc-beat-dur-ms []
  (* (calc-beat-dur) 1000))

(defn quant-to-timestamp
  "Given a quant (e.g. the off-beat of one would be '1.5'),
  beat duartion, and offset returns number timestamp in
  milliseconds from the offset"
  [quant offset beat-duration]
    (+ offset (* (- quant 1) (calc-beat-dur-ms))))

(defn- play-event [event]
  ((:instr event)
   :freq (:freq event)
   :amp (:amp event)))

(defn- schedule-measure [measure next-bar-timestamp]
  (doseq [[quant events] (partition 2 measure)]
    (let [timestamp (quant-to-timestamp quant next-bar-timestamp (calc-beat-dur))]
      (at timestamp
          (doseq [event events]
            (play-event event))))))

;; measures are scheduled one beat before they begin
(defn- schedule-cycle [measures current-time]
  (let [next-measure (first measures)
        remaining-measures (rest measures)
        beat-dur-ms (calc-beat-dur-ms)
        next-bar-timestamp (+ current-time beat-dur-ms)
        next-cycle-timestamp (+ current-time (* beat-dur-ms beats-per-bar))]

    (schedule-measure next-measure next-bar-timestamp)

    (if-not (empty? remaining-measures)
      (apply-at next-cycle-timestamp
                #'schedule-cycle
                [remaining-measures next-cycle-timestamp]))))

(defn play-score [score]
  (binding [beats-per-minute (:bpm score)
            beats-per-bar (:bpb score)]
    (schedule-cycle (:measures score) (now))))

(defn sin-blip [freq]
  {:instr sin-synth
   :freq freq
   :amp 0.1
   :outbus 0})

(def score
  {:bpm 120
   :bpb 3
   :measures [[1.0 [(sin-blip 440)]
               2.0 [(sin-blip 220)]
               3.0 [(sin-blip 220)]]

              [1.0 [(sin-blip 440)]
               2.0 [(sin-blip 220)]
               3.0 [(sin-blip 220)]]]})

(play-score score)
