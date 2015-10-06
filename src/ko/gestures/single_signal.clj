(ns ko.gestures.single_signal
  [:use
   [ko.synth_defs.single_signal]
   [ko.gesture]
   [ko.scheduling]]
  [:require
   [overtone.core :as ot]]
  (:gen-class))

(deftype SingleSynthGesture [g-name spec node-vec-atom]
  IGesture
  (g-nodes [this] @node-vec-atom)

  (g-start [this]
    (let [synth-args (flatten (into [] (dissoc spec :instr)))
          synth-id (apply (:instr spec) synth-args)]
      (swap! node-vec-atom conj synth-id)))

  (g-ctl [this & specs]
    (let [target (@node-vec-atom g-name)]
      (apply ot/ctl (conj specs target))))

  (g-end [this gesture-map-atom]
    (let [target (g-nodes this)]
      (ot/kill target)
      (remove-from-atom-map gesture-map-atom g-name))))

(defn ssg [g-name spec gesture-map-atom]
  (let [g-instance (BasicGesture. g-name spec (atom []))]
    (swap! gesture-map-atom #(assoc % g-name g-instance))))
