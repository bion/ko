(ns ko.gestures.single_signal
  [:use
   [ko.synth_defs.single_signal]
   [ko.gesture]
   [ko.scheduling]]
  [:require
   [overtone.core :as ot]]
  (:gen-class))

(deftype GSingleSynth [g-name spec node-vec-atom]
  IGesture
  (g-nodes [this] @node-vec-atom)
  (get-g-name [this] g-name)

  (g-start [this]
    (let [synth-args (flatten (into [] (dissoc spec :instr)))
          synth-id (apply (:instr spec) synth-args)]
      (swap! node-vec-atom conj synth-id)))

  (g-ctl [this spec]
    (apply ot/ctl (apply conj @node-vec-atom spec)))

  (g-end [this] @node-vec-atom)

(defn ssg [g-name spec gesture-map-atom]
  (if (nil? (:instr spec))
    (throw (Exception. (str "no instr specified in `ssg` with `spec`:" spec)))
    (let [g-instance (GSingleSynth. g-name spec (atom []))]
      (swap! gesture-map-atom #(assoc % g-name g-instance))
      g-instance)))
