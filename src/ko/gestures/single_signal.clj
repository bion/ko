(ns ko.gestures.single_signal
  [:use
   [ko.synth_defs.single_signal]
   [ko.gesture]
   [ko.scheduling]]
  [:require
   [overtone.core :as ot]]
  (:gen-class))

;; returns a function that when called begins playing the gesture
;; and returns a representation of the playing gesture
(defn ssg-gest
  [gesture mutations]
  (let [spec (:spec gesture)]
    (if (nil? (:instr spec))
      (throw (Exception. (str "no instr specified in `ssg` gesture"
                              " with `spec`: " spec))))

    (let [synth-args (flatten (into [] (dissoc spec :instr)))]
      (fn []
        (let [synth-id (apply (:instr spec) synth-args)]
          synth-id)))))

(deftype GSingleSynth [spec mutations internal-nodes]
  IGesture
  (g-nodes [this] (flatten (values @internal-nodes)))

  (g-prep [this]
    (swap! internal-nodes assoc :public-synths [])
    (let [control-synths (gen-control-synths [(:instr spec)] mutations)]))

  (g-start [this]
    (let [synth-args (flatten (into [] (dissoc spec :instr)))
          synth-id (apply (:instr spec) synth-args)]
      (swap! internal-nodes update-in [:public-synths] conj synth-id)))

  (g-ctl [this spec]
    (apply ot/ctl (apply conj (g-nodes this) spec)))

  (g-end [this] (g-nodes this)))
