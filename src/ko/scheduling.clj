(ns ko.scheduling
  [:require [overtone.core :as ot]]
  [:use [ko.gesture]]
  (:gen-class))

(defn remove-from-atom-map [atom-map k]
  (swap! atom-map
         (fn [atom-map-val]
           (apply dissoc (into [atom-map-val] k)))))

(def beats-per-bar 4)
(def beats-per-minute 120)
(def living-gestures-map-atom (atom {}))

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

(defn- schedule-measure
  "Each measure is a vector containing paired quant (decimal beat)
  and gesture data. Schedule measure's gesture's at their corresponding quant."
  [measure next-bar-timestamp]
  (doseq [[quant events] (partition 2 measure)]
    (let [timestamp (quant-to-timestamp quant next-bar-timestamp (calc-beat-dur))]
      (ot/at timestamp
             (doseq [event events] (event))))))

(defn schedule-cycle
  "Implements the temporal recursion pattern (see `(doc apply-at`)). Recurses
  through provided sequence of measures, scheduling each one beat before it begins
  until all of have been scheduled."
  [measures current-time]
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

(defmacro begin
  "Send start message to a gesture"
  [g-name gesture]
  `(let [g-instance# ~gesture]
     #((prn (str "begin " ~g-name))
       (swap! living-gestures-map-atom (fn [lgm#] (assoc lgm# ~g-name g-instance#)))
       (g-start g-instance#))))

(defn adjust
  "Send control messages to a running gesture.
  Messages are specified as alternating argument key value pairs"
  [g-name & rest]
  #(let [gesture (@living-gestures-map-atom g-name)]
     (prn (str "adjust " g-name))
     (g-ctl gesture rest)))

(defn finish
  "Send end message to gestures and remove from `living-gestures-map-atom`"
  [& g-names]
  #(let [gestures @living-gestures-map-atom]
     (prn (apply str (concat ["finish "] g-names)))
     (doseq [g-name g-names]
       (remove-from-atom-map living-gestures-map-atom g-name)
       (ot/kill (g-end (g-name gestures))))))

(defn play-score [score]
  (schedule-cycle (:measures score) (ot/now)))