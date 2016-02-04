(ns ko.scheduling
  [:require [overtone.core :as ot]]
  (:gen-class))

(defn calc-beat-dur-ms [beat-dur]
  (* beat-dur 1000))

(defn quant-to-timestamp
  "Given a quant (e.g. the off-beat of one would be '1.5'),
  beat duartion, and offset returns number timestamp in
  milliseconds from the offset"
  [quant offset beat-dur]
  (+ offset
     (* (- quant 1)
        (calc-beat-dur-ms beat-dur))))

(defn- schedule-measure
  "Each measure is a vector containing paired quant (decimal beat)
  and gesture data. Schedule measure's gesture's at their corresponding quant."
  [measure next-bar-timestamp beat-dur]
  (doseq [[quant events] measure]
    (let [timestamp (quant-to-timestamp quant next-bar-timestamp beat-dur)]
      (ot/at timestamp
             (doseq [event events] (event))))))

(defn- schedule-cycle
  "Implements the temporal recursion pattern (see `(doc apply-at)`). Recurses
  through provided sequence of measures, scheduling each one beat before it begins
  until all of them have been scheduled."
  [measures current-time]
  (let [next-measure (first measures)
        remaining-measures (rest measures)
        {:keys [beat-dur beats-per-bar]} (meta next-measure)
        beat-dur-ms (calc-beat-dur-ms beat-dur)
        next-bar-timestamp (+ current-time beat-dur-ms)
        next-cycle-timestamp (+ current-time (* beat-dur-ms beats-per-bar))]

    (schedule-measure next-measure next-bar-timestamp beat-dur)

    (if-not (empty? remaining-measures)
      (ot/apply-at next-cycle-timestamp
                   schedule-cycle
                   [remaining-measures next-cycle-timestamp]))))

(defn play-score [score]
  (schedule-cycle score (ot/now)))
