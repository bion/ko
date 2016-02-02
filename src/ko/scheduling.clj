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
  (doseq [[quant events] measure]
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

;; clear previous definition of multimethod
(def begin nil)
(defmulti begin
  "Begin playing a gesture. `begin` events can take one
  of several types.

  Single-synth Gestures (ssg) take the form

  (begin :ssg :gesture-name spec)

  where `spec` must be a map containing a :instr key specifying
  a ko-synthdef along with all other params to the synth. Spec
  can itself be a map, a var referring to a map, or form that when
  evaluated returns a map."
  (fn [type & args]
    type))

(defmethod begin :ssg
  [g-type g-name spec & mutations]
  (let [g-instance (ssg-gest spec (first mutations))]
    #(do
       (prn (str "playing " g-name))
       (let [g-nodes (g-instance)]
         (swap! living-gestures-map-atom
                (fn [lgm] (assoc lgm g-name g-nodes)))))))

(defn adjust
  "Send control messages to a running gesture.
  Messages are specified as alternating argument key value pairs"
  [g-name & rest]
  #(do
     (prn (str "adjust " g-name))
     (let [g-nodes (@living-gestures-map-atom g-name)] ;; assumes nodes are stored here
       (apply ot/ctl (apply conj g-nodes rest)))))

(defn finish
  "Send end message to gestures and remove from `living-gestures-map-atom`"
  [& g-names]
  #(do
     (prn "finish " g-names)
     (doseq [node (map @living-gestures-map-atom g-names)]
       (ot/kill node))
     (remove-from-atom-map living-gestures-map-atom g-names)))

(defn play-score [score]
  (schedule-cycle score (ot/now)))
