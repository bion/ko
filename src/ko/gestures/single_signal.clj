(ns ko.gestures.single_signal
  [:use
   [ko.synth_defs.single_signal]
   [ko.gesture]
   [ko.scheduling]]
  [:require
   [overtone.core :as ot]]
  (:gen-class))

(deftype GSingleSynth [spec node-vec-atom]
  IGesture
  (g-nodes [this] @node-vec-atom)

  (g-start [this]
    (let [synth-args (flatten (into [] (dissoc spec :instr)))
          synth-id (apply (:instr spec) synth-args)]
      (swap! node-vec-atom conj synth-id)))

  (g-ctl [this spec]
    (apply ot/ctl (apply conj @node-vec-atom spec)))

  (g-end [this] @node-vec-atom))

(defn ssg [spec]
  (if (nil? (:instr spec))
    (throw (Exception. (str "no instr specified in `ssg` with `spec`:" spec)))
    (GSingleSynth. spec (atom []))))
