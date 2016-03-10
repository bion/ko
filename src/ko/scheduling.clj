(ns ko.scheduling
  [:require [overtone.core :as ot]])

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
  [measure next-bar-timestamp beat-dur living-gestures]

  (doseq [[quant actions] measure]
    (let [timestamp (quant-to-timestamp quant next-bar-timestamp beat-dur)]
      (ot/at timestamp
             (doseq [action actions] (action living-gestures))))))

(defn resolve-next-index [score index]
  (let [next-index             (inc index)
        {:keys [labels jumps]} (meta score)
        jump                   (get jumps next-index)]

    (if jump
      (let [{:keys [label should-jump?]} jump]
        (if (should-jump?) (labels label) next-index))
      next-index)))

(defn- schedule-cycle
  "Implements the temporal recursion pattern (see `(doc apply-at)`). Recurses
  through provided sequence of measures, scheduling each one beat before it begins
  until all of them have been scheduled."
  [measures measure-index current-time]

  (let [living-gestures                  (:living-gestures (meta measures))
        measure                          (measures measure-index)
        {:keys [beat-dur beats-per-bar]} (meta measure)
        beat-dur-ms                      (calc-beat-dur-ms beat-dur)
        next-bar-timestamp               (+ current-time beat-dur-ms)
        next-cycle-timestamp             (+ current-time (* beat-dur-ms beats-per-bar))
        next-measure-index               (resolve-next-index measures measure-index)]

    (schedule-measure measure next-bar-timestamp beat-dur living-gestures)

    (if (get measures next-measure-index)
      (ot/apply-at next-cycle-timestamp
                   schedule-cycle
                   [measures next-measure-index next-cycle-timestamp]))))

(defn play-score [score]
  (schedule-cycle score 0 (ot/now)))
