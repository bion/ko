(ns ko.core
  [:require [overtone.core :as ot]]
  (:gen-class))

(ot/defsynth sin-synth [amp 1 freq 440 outbus 0 action 2]
  (ot/out outbus
          (* amp
             (ot/sin-osc freq)
             (ot/env-gen:kr
              (ot/envelope [0 1 1]
                           [0.01 0.3]
                           [-1 -1])
              :action action))))

(def beats-per-bar 4)
(def beats-per-minute 120)
(def running-synths (atom {}))

(defn calc-beat-dur []
  (/ 60.0 beats-per-minute))

(defn calc-beat-dur-ms []
  (* (calc-beat-dur) 1000))

(defn quant-to-timestamp
  "Given a quant (e.g. the off-beat of one would be '1.5'),
  beat duartion, and offset returns number timestamp in
  milliseconds from the offset"
  [quant offset beat-duration]
  (+ offset
     (* (- quant 1)
        (calc-beat-dur-ms))))

(defn- schedule-measure [measure next-bar-timestamp]
  (doseq [[quant events] (partition 2 measure)]
    (let [timestamp (quant-to-timestamp quant next-bar-timestamp (calc-beat-dur))]
      (ot/at timestamp
             (doseq [event events] (event))))))

;; measures are scheduled one beat before they begin
(defn- schedule-cycle [measures current-time]
  (let [next-measure (first measures)
        remaining-measures (rest measures)
        beat-dur-ms (calc-beat-dur-ms)
        next-bar-timestamp (+ current-time beat-dur-ms)
        next-cycle-timestamp (+ current-time (* beat-dur-ms beats-per-bar))]

    (schedule-measure next-measure next-bar-timestamp)

    (if-not (empty? remaining-measures)
      (ot/apply-at next-cycle-timestamp
                   schedule-cycle
                   [remaining-measures next-cycle-timestamp]))))

(defn play-score [score]
  (schedule-cycle (:measures score) (ot/now)))

(defn- play-event [event]
  ((:instr event)
   :freq (:freq event)
   :amp (:amp event)
   :action (:action event)))

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

(defn adjust
  "Send control messages to a running synth.
  Messages are specified as alternating argument key value pairs"
  [name & rest]
  #(let [target (@running-synths name)]
     (apply ot/ctl (conj rest target))))

(defn finish
  "Kill synth nodes and remove from `running-synths` atom"
  [& synth-names]
  #(let [synths-to-kill ((apply juxt synth-names) @running-synths)]
     (ot/kill synths-to-kill)
     (swap! running-synths
            (fn [running-synths-val]
              (apply dissoc (into [running-synths-val] synth-names))))))

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
